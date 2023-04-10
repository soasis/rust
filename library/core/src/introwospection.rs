//! Traits for language-level compile-time reflection facilities (also sometimes
//! referred to as "introspection".) Compile-time reflection is built on the idea
//! that the language itself is providing every single bit of information it
//! possibly can (without directly leaking tokens or AST information) in a
//! highly structured manner that resembles language constructs (but may not
//! map cleanly to compiler internals, requiring just a tiny bit of
//! compile-time data massaging to produce). The goal of all introspection
//! facilities is first and foremost the ability to act on the information of
//! the code itself. Most of the things presented here will be primitive as
//! compared to counterparts in languages such as C#, Java, Haskell, JS, Ruby,
//! or similar dynamic languages.
//!
//! In particular, the two core tenents are non-negotiable for this:
//! - it must not allocate, ever.
//! - it must not require runtime dispatch to use, ever.
//!
//! Users of this API may lower the concepts found here to dynamic alternatives,
//! and as part of this API there are foundamental elements describing the full
//! functionality but without all of the compile-time information (see the
//! `any_introwospection` module as part of the `std` crate).
//!
//! The explicit goal of this module is to provide the library components that
//! work with the language to give the full features of compile-time,
//! non-allocating reflection in Rust. Using the `introwospect` and
//! `introwospect_over` keywords, one is meant to do compile-time inspection
//! in conjunction with generic programming. This also allows users to do
//! things that `#[derive(...)]` macros would normally need to tag and
//! annotate their code with without needing to invade that crate or its
//! namespaces to provide similar or identical functionality.
//!
//! In this direction, the following Rust code would allow someone --
//! without allocating or using dynamic dispatch -- to walk over the
//! fields of a structure they do **not** own:
//!
//! ```
//! use std::introwospection::*;
//! use other_crate::Meow; // a struct of the form:
//! /*
//! pub struct Meow {
//!     pub purr_level: i32,
//!     hairballs: i32,
//!     pub scratch_couch: SystemTime
//! }
//! */
//!
//! fn main () {
//!     type MeowInfo = introwospect<other_crate::Meow>;
//!     println!("struct {}, with {} fields:\n\t{} ({}, {})\n\t{} ({}, {})",
//!         <MeowInfo as StructDescriptor>::NAME,
//!         <MeowInfo as StructDescriptor>::FIELD_COUNT,
//!         <MeowInfo as FieldDescriptor<0>>::FIELD_COUNT,
//!         std::any::type_name::<<MeowInfo as FieldDescriptor<0>>::Type>(),
//!         <MeowInfo as FieldDescriptor<0>>::BYTE_OFFSET,
//!         <MeowInfo as FieldDescriptor<1>>::NAME,
//!         std::any::type_name::<<MeowInfo as FieldDescriptor<1>>::Type>(),
//!         <MeowInfo as FieldDescriptor<1>>::BYTE_OFFSET);
//!     // Should display:
//!     /* struct other_create::Meow with 2 fields:
//!      *     purr_level (i32, 0)
//!      *     scratch_couch (std::time::SystemTime, 8)
//!     */
//! }
//! ```
//!
//! As you can see, we can use the `StructDescriptor` trait to access the
//! associated, compile-time constants `NAME` and `FIELD_COUNT` for a
//! structure in an outside crate. It also provides compile-time access to
//! each and every field on the type that is **visible** to the current
//! scope. This means that private fields stay inaccessible, such as
//! `hairballs` on the `Meow` struct from the `other_crate`. Of course, it
//! is tedious to program in this fashion: this is effectively hard-coding
//! the number of fields you can access by way of accessing each field
//! directly through a Fully Qualified path. Therefore, this can be
//! simplified through the use of visitors:
//!
//! ```
//! use std::introwospection::*;
//! use other_crate::Meow; // a struct of the form:
//! /*
//! pub struct Meow {
//!     pub purr_level: i32,
//!     hairballs: i32,
//!     pub scratch_couch: SystemTime
//! }
//! */
//!
//! struct DescriptorPrinter;
//! impl FieldDescriptorVisitor for DescriptorPrinter {
//!     type Output = ()
//!     fn visit_field<Type, const INDEX: usize>(&self)
//!         -> Self::Output
//!         where Type : FieldDescriptor<INDEX>
//!     {
//!         let type_name = std::any::type_name::<Type::Type>();
//!         println!("\t{} ({}, {})\n" Type::NAME, type_name, Type::);
//!     }
//! }
//! impl StructDescriptorVisitor for DescriptorPrinter {
//!     type Output = ()
//!     fn visit_struct<Type>(&self)
//!         -> Self::Output
//!         where Type : StructDescriptor
//!     {
//!         let type_name = std::any::type_name::<Type::Type>();
//!         println!("struct {}, with {} fields:\n" Type::NAME, Type::FIELD_COUNT);
//!         ( introwospect_over(Type::Type, self) );
//!     }
//! }
//!
//! fn main () {
//!     type MeowInfo = introwospect<other_crate::Meow>;
//!     let printer = DescriptorPrinter;
//!     printer.visit_struct::<MeowInfo>();
//!     // Should display:
//!     /* struct other_create::Meow with 2 fields:
//!      *     purr_level (i32, 0)
//!      *     scratch_couch (std::time::SystemTime, 8)
//!     */
//! }
//! ```

#![unstable(feature = "introwospection", issue = "none")]

use crate::any::{Any, TypeId};
use crate::fmt::{Debug, Display, Formatter, Result};
use crate::mem::Discriminant;
use crate::option::Option;

/// The Abstract Data Type (ADT) identifier for determining the kind of ADT being
/// reflected over. Included as part of anything that implements ghe `AdtDescriptor`
/// trait.
#[repr(u8)]
pub enum AdtId {
    /// An abstract data type made using the `struct` keyword.
    Struct,
    /// An abstract data type made using the `union` keyword. Most
    /// of the offsets for a union (or for a `#[repr(C)]` enumeration)
    /// should be at or close to 0, and the discriminant is generally
    /// managed by the user rather than the compiler, which makes it
    /// an unsafe construct.
    Union,
    /// An abstract data type made using the `enum` keyword. It contains 0 or
    /// more variants which are fields directly related to the enumeration itself,
    /// and whose offsets are calculcated as from the base of the enumeration,
    /// and NOT as an independent data type.
    Enum,
    /// A tuple, created by a list of initializers or types enclosed by parentheses
    /// (e.g., `( ... )`). Note that the `Unit` type is just the empty tuple, and so
    /// is not listed as a separate type in this list.
    Tuple,
    /// An array type, usually formed through initializer with square brackets. The type is
    /// a type and a fixed size `N`, e.g. `[ ...; N ]`.
    Array,
    /// A slice type, usually formed through initializer with square brackets no fixed size
    /// , e.g. `[ ... ]`.
    Slice,
    /// A function definition, such as those defined with the `fn` keyword.
    Function,
}

/// An empty structure whose sole job is to indicate when a variant is unspecified.
/// This is important in the case of two variants of e.g. an
/// enumeration which only different by the use of nothing and the
/// use of () in the variants. That is:
/// ````
/// enum E0 { A }
/// enum E1 { A() }
/// ````
/// are two entirely separate constructs with different meanings. There
/// is no in-language type description for the `E0::A`, while `E1::A`'s
/// "field" is just the unit type `()`.
///
/// Structures such as
/// ```
/// struct S0;
/// struct S1();
/// struct S1{};
/// ```
/// may also, apparently, have subtle differences, but this is indicated
/// elsewhere in the API by field counts and whether or not those fields
/// are anonymous (their `NAME` is empty) or have names.
pub struct NoType;

/// A list of names and values of attributes contained within the
/// `#[introwospection(...)]` attribute.
pub struct AttributeDescriptor {
    /// The name of an attribute. Required, comes before the `=`
    /// sign (if present).
    pub name: &'static str,
    /// The optional value of the attribute. Optional, comes
    /// after the `=` sign (if present). Does not include the
    /// first matching set of quotation marks that delimit it.
    pub value: Option<&'static str>,
}

/// The basic Abstract Data Type (ADT) descriptor.
pub trait AdtDescriptor {
    /// The identifying ID of an abstract data type, for matching on whether it's a function,
    /// structure, and similar useful identifications.
    const ID: AdtId;
    /// The name of the abstract data type. This is meant to be the full path of the data type,
    /// according to the version of Rust this was compiled again.
    const NAME: &'static str;
    /// The introwospection attributes (`#[introwospection(...)`]) attached to this entity.
    /// These are attributes capable of being used for compile-time introspection, such as for
    /// marking a field as non-serializable or noting specific intended behaviors for a function
    /// definition and the processing of its arguments.
    ///
    /// NOTE
    /// Only `introwospection` attributes are collected here. Other attributes are not,
    /// as it is important for the author of a specific data type, function, or field to have
    /// penultimate control of such attributes. (Individuals writing `*Visitor` types may alter
    /// or ignore behavior for attributes, which gives final control to the individual writing
    /// such visitor methods.)
    const ATTRIBUTES: &'static [AttributeDescriptor] = &[];
}

/// A description of a structure or union type.
pub trait StructDescriptor: AdtDescriptor {
    /// The type of the `struct` or `union` that was described.
    type Type;
    /// The number of fields for this structure or union type.
    const FIELD_COUNT: usize = 0;
}

/// A description of an enumeration type.
pub trait EnumDescriptor: AdtDescriptor {
    /// The type of the `enum` that was described.
    type Type;
    /// A type describing all of the variants of this enumeration.
    ///
    /// NOTE
    /// TODO(thephd) Enable a succint way to describe all of the constraints on this type:
    ///
    /// ```
    /// type VariantsType :
    ///     (for <const I: usize = 0..Self::VARIANT_COUNT> VariantDescriptor<I>)
    /// = NoType;
    /// ```
    /// doing this would allow it to be acted upon in a meaningful fashion by generic code,
    /// but such bounds/constraint technology does not exist yet.
    type VariantsType = NoType;
    /// The number of variants for this enumeration.
    const VARIANT_COUNT: usize = 0;
}

/// A description of a function definition or similar construct.
pub trait FunctionDescriptor: AdtDescriptor {
    /// A type describing all of the parmaeters of this function. If this is `NoType`, then
    /// there were no parameters that was part of this function.
    ///
    /// NOTE
    /// TODO(thephd) Enable a succint way to describe all of the constraints on this type:
    /// ```
    /// type ParametersType :
    ///     (for <const I: usize = 0..Self::PARAMETER_COUNT> ParameterDescriptor<I>)
    /// = NoType;
    /// ```
    /// to specify the proper boundaries to make this type usable in generic contexts. (This is
    /// bikeshed syntax and subject to change, as there is already a `for <T>` feature in Rust.)
    type ParametersType = NoType;
    /// The return type of of the function.
    type ReturnType;
    /// The number of parameters in the function. Note that a pattern constitues a
    /// single parameter.
    const PARAMETER_COUNT: usize = 0;
}

/// A parameter for a function definition, or similar construction.
pub trait ParameterDescriptor<const PARAMETER_INDEX: usize> {
    /// The function type related to this parameter descriptor.
    type OwnerType;
    /// The type of the function parameter.
    type Type;
    /// The 0-based declaration (source code) index.
    const PARAMETER_INDEX: usize = PARAMETER_INDEX;
    /// The name of the parameter in the function. This may be empty, as would be the case for a
    /// function declaration that contains a destructuring that breaks the parameter down into the
    /// constituent parts with destructing to match a pattern.
    const NAME: &'static str;
    /// The introwospection attributes (`#[introwospection(...)`]) attached to this entity.
    /// These are attributes capable of being used for compile-time introspection, such as for
    /// marking a field as non-serializable or noting specific intended behaviors for a function
    /// definition and the processing of its arguments.
    ///
    /// NOTE
    /// Only `introwospection` attributes are collected here. Other attributes are not,
    /// as it is important for the author of a specific data type, function, or field to have
    /// penultimate control of such attributes. (Individuals writing `*Visitor` types may alter
    /// or ignore behavior for attributes, which gives final control to the individual writing
    /// such visitor methods.)
    const ATTRIBUTES: &'static [AttributeDescriptor] = &[];
}

/// A descriptor that describes all the necessary information of a field that exists on the variant
/// of an enumeration, a field on a structure, or a field on a union.
///
/// `DECLARATION_INDEX` is the 0-based index of the field in declaration (source code) order.
pub trait FieldDescriptor<const DECLARATION_INDEX: usize> {
    /// The type that owns this field. It may be any abstract data type union, a variant,
    /// or a structure type. All (byte) offsets are from the base of an `Self::OwnerType`
    /// object.
    type OwnerType;
    /// The data type of the field itself.
    type Type;
    /// The 0-based declaration (source code) index.
    const DECLARATION_INDEX: usize = DECLARATION_INDEX;
    /// The name of the field within the union, variant, or structure. If this is empty, it
    /// signifies an completely unnamed field. If this is part of a tuple-like field syntax,
    /// then the name of the field will not be empty, but instead be `.0` or similar.
    const NAME: &'static str;
    /// The byte offset from the base of an owner type object to the data type of this field.
    const BYTE_OFFSET: usize;
    /// The introwospection attributes (`#[introwospection(...)`]) attached to this entity.
    /// These are attributes capable of being used for compile-time introspection, such as for
    /// marking a field as non-serializable or noting specific intended behaviors for a function
    /// definition and the processing of its arguments.
    ///
    /// NOTE
    /// Only `introwospection` attributes are collected here. Other attributes are not,
    /// as it is important for the author of a specific data type, function, or field to have
    /// penultimate control of such attributes. (Individuals writing `*Visitor` types may alter
    /// or ignore behavior for attributes, which gives final control to the individual writing
    /// such visitor methods.)
    const ATTRIBUTES: &'static [AttributeDescriptor] = &[];
}

/// A descriptor that describes all the necessary components of a variant, from its
/// names to its fields, at compile-time.
///
/// `DECLARTION_INDEX` is the index of the variant in declaration (source code) order.
pub trait VariantDescriptor<const DECLARATION_INDEX: usize> {
    /// The type which owns this variant.
    type OwnerType: EnumDescriptor;
    /// A type that represents the fields of this enumeration. If this is `core::introwospection::NoType`,
    /// then it has no implementations of a field.
    ///
    /// NOTE
    /// TODO(thephd) Enable a succint way to describe all of the constraints on this type:
    /// ```
    /// type FieldsType :
    ///     (for <const I: usize = 0..Self::FIELD_COUNT> FieldDescriptor<I>)
    /// = NoType;
    /// ```
    /// to specify the proper boundaries to make this type usable in generic contexts. (This is
    /// bikeshed syntax and subject to change, as there is already a `for <T>` feature in Rust.)
    type FieldsType = NoType;
    /// The integer type that is used for this declaration if it was declared with the representation
    /// attribute, `#[repr(IntType)]`. Used in conjunction with the `INTEGER_VALUE` associated
    /// `const` item.
    type IntType = NoType;
    /// The 0-based index of the variant in declaration (source code) order.
    const DECLARATION_INDEX: usize = DECLARATION_INDEX;
    /// The name of the variant within the enumeration.
    const NAME: &'static str;
    /// The discriminant that identifies this variant of the data type. The discriminant can
    /// be used when looping over all fields to find which variant of an enumeration is the
    /// currently active variant. Then, the `FieldsType` or `INTEGER_VALUE` -- if present --
    /// can be used to deduce the fields at the specific offset from an object of the enumeration
    /// type, or can be used to get the constant integer value of this variant in the enumeration,
    /// respectively.
    ///
    /// NOTE(
    /// TODO(thephd)) Enable supporting the intrinsic:
    /// ```
    /// const DISCRIMINENT : Discriminant<Self::OwnerType> =
    ///      std::mem::discriminant_at<Self::OwnerType>(Self::DECLARATION_INDEX);
    /// ```
    /// to get a discriminant at compile-time without needing to generate a fake
    /// enumeration object.
    const DISCRIMINANT: Discriminant<Self::OwnerType>;
    /// The number of field desciptors associated with the `FieldsType` type and this variant.
    /// `FIELD_COUNT` and `FielsdType` can be used If `FIELD_COUNT` is zero
    const FIELD_COUNT: usize = 0;
    /// The value of an enumeration which opts into a `#[repr(IntType)]` representation.
    /// If the enumeration has not opted into such a representation, then this will be
    /// `None`. Otherwise, `Self::IntType` will be set to the integer type specified in the
    /// representation attribute and the value of the enumeration will be stored here.
    const INTEGER_VALUE: Option<Self::IntType> = None;
    /// The introwospection attributes (`#[introwospection(...)`]) attached to this entity.
    /// These are attributes capable of being used for compile-time introspection, such as for
    /// marking a field as non-serializable or noting specific intended behaviors for a function
    /// definition and the processing of its arguments.
    ///
    /// NOTE
    /// Only `introwospection` attributes are collected here. Other attributes are not,
    /// as it is important for the author of a specific data type, function, or field to have
    /// penultimate control of such attributes. (Individuals writing `*Visitor` types may alter
    /// or ignore behavior for attributes, which gives final control to the individual writing
    /// such visitor methods.)
    const ATTRIBUTES: &'static [AttributeDescriptor] = &[];
}

/// A visitor on a `StructDescriptor` trait implementation, to handle the compile-time
/// data stored on such a trait implementation.
pub trait StructDescriptorVisitor {
    /// The return type of the `visit_enum` and `visit_enum_mut` implementations.
    type Output;

    /// A visitation function for a specific `StructDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_struct<Type>(&self) -> Self::Output
    where
        Type: StructDescriptor;

    /// A visitation function for a specific `StructDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_struct::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_struct_mut<Type>(&mut self) -> Self::Output
    where
        Type: StructDescriptor,
    {
        return Self::visit_struct::<Type>(&self);
    }
}

/// A visitor on a `EnumDescriptor` trait implementation, to handle the compile-time
/// data stored on such a trait implementation.
pub trait EnumDescriptorVisitor {
    /// The return type of the `visit_enum` and `visit_enum_mut` implementations.
    type Output;

    /// A visitation function for a specific `EnumDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_enum<Type>(&self) -> Self::Output
    where
        Type: EnumDescriptor;

    /// A visitation function for a specific `EnumDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_enum::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_enum_mut<Type>(&mut self) -> Self::Output
    where
        Type: EnumDescriptor,
    {
        return Self::visit_enum::<Type>(&self);
    }
}

/// A visitor on a `FunctionDescriptor` trait implementation, to handle the compile-time
/// data stored on such a trait implementation.
pub trait FunctionDescriptorVisitor {
    /// The return type of the `visit_function` and `visit_function_mut` implementations.
    type Output;

    /// A visitation function for a specific `FunctionDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_function<Type>(&self) -> Self::Output
    where
        Type: FunctionDescriptor;

    /// A visitation function for a specific `FunctionDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_function::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_function_mut<Type>(&mut self) -> Self::Output
    where
        Type: FunctionDescriptor,
    {
        return Self::visit_function::<Type>(&self);
    }
}

/// A visitor for a collection of parameter descriptors that handles all incoming compile-time
/// data stored on an implementation of a typical `ParameterDescriptor<I>`, where `I` is from `0`
/// to the maximum parameter count of a given type. This is used for function types.
pub trait ParameterDescriptorVisitor {
    /// The return type of the `visit_parameter` and `visit_parameter_mut` implementations.
    type Output;

    /// A visitation function for a specific `ParameterDescriptor` type.
    ///
    /// Returns `Self::Output`.
    fn visit_parameter<Type, const DECLARATION_INDEX: usize>(&self) -> Self::Output
    where
        Type: ParameterDescriptor<DECLARATION_INDEX>;

    /// A visitation function for a specific `ParameterDescriptor` type. This form is mutable,
    /// and by default calls `Self::visit_parameter::<Type, DECLARATION_INDEX>`.
    ///
    /// Returns `Self::Output`
    fn visit_parameter_mut<Type, const DECLARATION_INDEX: usize>(&mut self) -> Self::Output
    where
        Type: ParameterDescriptor<DECLARATION_INDEX>,
    {
        return Self::visit_parameter::<Type, DECLARATION_INDEX>(&self);
    }
}

/// A visitor for a collection of field descriptors that handles all incoming compile-time
/// data stored on an implementation of a typical `FieldDescriptor<I>`, where `I` is from `0`
/// to the maximum field count of a given type. This is used for structure and union types.
pub trait FieldDescriptorVisitor {
    /// The return type of the `visit_field` and `visit_field_mut` implementations.
    type Output;

    /// A visitation function for a specific `FieldDescriptor<DECLARATION_INDEX>`
    /// implementation.
    ///
    /// Returns `Self::Output`.
    fn visit_field<Type, const DECLARATION_INDEX: usize>(&self) -> Self::Output
    where
        Type: FieldDescriptor<DECLARATION_INDEX>;

    /// A visitation function for a specific `FieldDescriptor<DECLARATION_INDEX>` type. This
    /// form is mutable, and by default calls `Self::visit_field::<Type, DECLARATION_INDEX>`.
    ///
    /// Returns `Self::Output`1
    fn visit_field_mut<Type, const DECLARATION_INDEX: usize>(&mut self) -> Self::Output
    where
        Type: FieldDescriptor<DECLARATION_INDEX>,
    {
        return Self::visit_field::<Type, DECLARATION_INDEX>(&self);
    }
}

/// A visitor for a collection of variant descriptors that handles all incoming compile-time
/// data stored on an implementation of a typical `VariantDescriptor<I>`, where `I` is from `0`
/// to the maximum variant count of a given type. This is used for enumeration types.
pub trait VariantDescriptorVisitor {
    /// The return type of the `visit_function` and `visit_function_mut` implementations.
    type Output;

    /// A visitation function for a specific `VariantDescriptor<DECLARATION_INDEX>`
    /// implementation.
    ///
    /// Returns `Self::Output`.
    fn visit_variant<Type, const DECLARATION_INDEX: usize>(&self) -> Self::Output
    where
        Type: VariantDescriptor<DECLARATION_INDEX>;

    /// A visitation function for a specific `VariantDescriptor<DECLARATION_INDEX>` type.
    /// This form is mutable, and by default calls
    /// `Self::visit_variant::<Type, DECLARATION_INDEX>`.
    ///
    /// Returns `Self::Output`
    fn visit_variant_mut<Type, const DECLARATION_INDEX: usize>(&mut self) -> Self::Output
    where
        Type: VariantDescriptor<DECLARATION_INDEX>,
    {
        return Self::visit_variant::<Type, DECLARATION_INDEX>(&self);
    }
}

/// A combination of all the other `core::introwospection::*Visitor` traits. This is
/// used to ensure a visitor implements all of the required visitors for every type
/// of entity, so that it can recurse down every type of reflectable entity.
pub trait DescriptorVisitor:
    FunctionDescriptorVisitor
    + ParameterDescriptorVisitor
    + StructDescriptorVisitor
    + EnumDescriptorVisitor
    + FieldDescriptorVisitor
    + VariantDescriptorVisitor
{
}

/// Returns a field of the given data type, offset from the owner type.
pub const fn get_field<Type, const DECLARATION_INDEX: usize>(owner: &Type::OwnerType) -> &Type::Type
where
    Type: FieldDescriptor<DECLARATION_INDEX>,
{
    unsafe {
        ((owner as *const Type::OwnerType as *const u8).add(Type::BYTE_OFFSET).cast::<Type::Type>())
            .as_ref()
            .unwrap_unchecked()
    }
}

/// Returns a field of the given data type, offset from the owner type.
pub const fn get_field_mut<Type, const DECLARATION_INDEX: usize>(
    owner: &mut Type::OwnerType,
) -> &mut Type::Type
where
    Type: FieldDescriptor<DECLARATION_INDEX>,
{
    unsafe {
        ((owner as *mut Type::OwnerType as *mut u8).add(Type::BYTE_OFFSET).cast::<Type::Type>())
            .as_mut()
            .unwrap_unchecked()
    }
}

/// A run-time description of a structure or union type.
#[derive(Debug)]
pub struct AnyStructDescriptor {
    /// The type of the `struct` or `union` that was described.
    pub type_id: TypeId,
    /// A slice describing each field of this `struct` or `union` type.
    pub fields: &'static [AnyFieldDescriptor],
}

/// A run-time description of an enumeration type.
#[derive(Debug)]
pub struct AnyEnumDescriptor {
    /// The type of the `enum` that was described.
    pub type_id: TypeId,
    /// A slice describing each variant of this `enum` type.
    pub variants: &'static [AnyVariantDescriptor],
}

/// A run-time description of a function definition, or similar construct.
#[derive(Debug)]
pub struct AnyFunctionDescriptor {
    /// The return type of this function.
    pub return_type: AnyStructDescriptor,
    /// A list of parameters that make up this function call.
    pub parameters: &'static [AnyParameterDescriptor],
}

/// A parameter in a function declaration or similar construct.
#[derive(Debug)]
pub struct AnyParameterDescriptor {
    /// A description fo the function tyoe that is related to this parameter.
    pub owner_type: AnyFunctionDescriptor,
    /// The type of the function parameter.
    pub type_id: TypeId,
    /// The 0-based declaration (source code) index for the parameter.
    pub parameter_index: usize,
    /// The name of the parameter in the function. This may be empty, as would be the case for a
    /// function declaration that contains a destructuring that breaks the parameter down into the
    /// constituent parts to match a pattern.
    pub name: &'static str,
    /// The introwospection attributes (`#[introwospection(...)`]) attached to this entity.
    /// These are attributes capable of being used for compile-time introspection, such as for
    /// marking a field as non-serializable or noting specific intended behaviors for a function
    /// definition and the processing of its arguments.
    ///
    /// NOTE: only `introwospection` attributes are collected here. Other attributes are not,
    /// as it is important for the author of a specific data type, function, or field to have
    /// penultimate control of such attributes. (Individuals writing `*Visitor` types may alter
    /// or ignore behavior for attributes, which gives final control to the individual writing
    /// such visitor methods.)
    pub attributes: &'static [AttributeDescriptor],
}

/// A descriptor that describes all the necessary information of a field that exists on the variant
/// of an enumeration, a field on a structure, or a field on a union.
#[derive(Debug)]
pub struct AnyFieldDescriptor {
    /// The type that owns this field. It may be any abstract data type union, a variant,
    /// or a structure type. All (byte) offsets are from the base of an `Self::OwnerType`
    /// object.
    pub owner_type: TypeId,
    /// The data type of the field itself.
    pub type_id: TypeId,
    /// The 0-based declaration (source code) index.
    pub declaration_index: usize,
    /// The name of the field within the union, variant, or structure. If this is empty, it
    /// signifies an completely unnamed field. If this is part of a tuple-like field syntax,
    /// then the name of the field will not be empty, but instead be `.0` or similar.
    pub name: &'static str,
    /// The byte offset from the base of an owner type object to the data type of this field.
    pub byte_ofset: usize,
    /// The introwospection attributes (`#[introwospection(...)`]) attached to this entity.
    /// These are attributes capable of being used for compile-time introspection, such as for
    /// marking a field as non-serializable or noting specific intended behaviors for a function
    /// definition and the processing of its arguments.
    ///
    /// NOTE: only `introwospection` attributes are collected here. Other attributes are not,
    /// as it is important for the author of a specific data type, function, or field to have
    /// penultimate control of such attributes. (Individuals writing `*Visitor` types may alter
    /// or ignore behavior for attributes, which gives final control to the individual writing
    /// such visitor methods.)
    pub attributes: &'static [AttributeDescriptor],
}

/// A descriptor that describes all the necessary components of a variant, from its
/// names to its fields, at compile-time.
#[derive(Debug)]
pub struct AnyVariantDescriptor {
    /// The enumeration that owns this variant.
    pub owner: AnyEnumDescriptor,
    /// The 0-based index of the variant in declaration (source code) order.
    pub declaration_index: usize,
    /// The name of the variant within the enumeration.
    pub name: &'static str,
    /// The discriminant that identifies this variant of the data type. The discriminant can
    /// be used when looping over all fields to find which variant of an enumeration is the
    /// currently active variant. Then, the `FieldsType` or `INTEGER_VALUE` -- if present --
    /// can be used to deduce the fields at the specific offset from an object of the enumeration
    /// type, or can be used to get the constant integer value of this variant in the enumeration,
    /// respectively.
    //const DISCRIMINANT: Discriminant<Self::OwnerType>,
    pub fields: &'static [AnyFieldDescriptor],
    /// A type-erased reference to a `Discriminant<T>` type. A user should check the `owner.type_id`
    /// parameter to verify the proper type `T` for the containing data type, then cast this to
    /// the appropriate `Discriminant<T>` to use.
    pub discriminant: &'static dyn Any,
    /// A type-erased reference to the value of an enumeration which opts into a `#[repr(IntType)]`
    /// representation.
    pub integer_value: &'static dyn Any,
    /// The introwospection attributes (`#[introwospection(...)`]) attached to this entity.
    /// These are attributes capable of being used for compile-time introspection, such as for
    /// marking a field as non-serializable or noting specific intended behaviors for a function
    /// definition and the processing of its arguments.
    ///
    /// NOTE: only `introwospection` attributes are collected here. Other attributes are not,
    /// as it is important for the author of a specific data type, function, or field to have
    /// penultimate control of such attributes. (Individuals writing `*Visitor` types may alter
    /// or ignore behavior for attributes, which gives final control to the individual writing
    /// such visitor methods.)
    pub attributes: &'static [AttributeDescriptor],
}

impl AdtDescriptor for NoType {
    const ID: AdtId = AdtId::Struct;
    const NAME: &'static str = "core::introwospection::NoType";
}

impl StructDescriptor for NoType {
    type Type = NoType;
}

impl Debug for NoType {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(self, f)
    }
}

impl Display for NoType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(<Self as AdtDescriptor>::NAME, f)
    }
}

impl Debug for AttributeDescriptor {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(self, f)
    }
}

impl Display for AttributeDescriptor {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.value {
            Some(value) => write!(f, "{} = \"{}\"", self.name, value),
            None => Display::fmt(self.name, f),
        }
    }
}

impl Debug for AdtId {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(self, f)
    }
}

impl Display for AdtId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            AdtId::Struct => Display::fmt("struct { ... }", f),
            AdtId::Enum => Display::fmt("enum { ... }", f),
            AdtId::Union => Display::fmt("union { ... }", f),
            AdtId::Tuple => Display::fmt("tuple ( ... )", f),
            AdtId::Array => Display::fmt("array [type; n]", f),
            AdtId::Slice => Display::fmt("slice [type]", f),
            AdtId::Function => Display::fmt("fn (...)", f),
        }
    }
}
