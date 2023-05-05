//! Traits and data types for language-level compile-time reflection facilities
//! (also sometimes referred to as "introspection".) Compile-time reflection is
//! built on the idea that the language itself is providing every single bit of
//! information it possibly can (without directly leaking tokens or AST
//! information) in a highly structured manner that resembles language
//! constructs (but may not map cleanly to compiler internals, requiring just
//! a tiny bit of compile-time data massaging to produce). The goal of all
//! introspection facilities is first and foremost the ability to act on the
//! information of the code itself. Most of the things presented here will be
//! primitive as compared to counterparts in languages such as C#, Java,
//! Haskell, JS, Ruby, or similar dynamic languages.
//!
//! In particular, the two core tenets that are non-negotiable for this:
//! - it must not perform runtime allocation, ever.
//! - it must not require runtime dispatch to use, ever.
//!
//! Users of this API may lower the concepts found here to dynamic alternatives,
//! and as part of this API there are fundamental elements describing the full
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
//! fields of a `struct` they do **not** own:
//!
//! ```
//! use std::introwospection::*;
//! use other_crate::Meow; // a struct of the form:
//! /*
//! pub struct Meow {
//!     pub purr_level: i32,
//!     hairballs: i32,
//!     pub scratch_couch: SystemTime,
//! }
//! */
//!
//! fn main () {
//!     type MeowInfo = introwospect_type<other_crate::Meow>;
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
//!      */
//! }
//! ```
//!
//! As you can see, we can use the `StructDescriptor` trait to access the
//! associated, compile-time constants `NAME` and `FIELD_COUNT` for a
//! data type in an outside crate. It also provides compile-time access to
//! each and every field on the type that is **visible** to the current
//! scope. This means that private fields stay inaccessible, such as
//! `hairballs` on the `Meow` struct from the `other_crate`. Of course, it
//! is tedious to program in this fashion: this is effectively hard-coding
//! the number of fields you can access by way of accessing each field
//! directly through a Fully Qualified path. To reduce the boilerplate,
//! this can be simplified through the use of visitors:
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
//!         println!("\t{} ({}, {})",
//!             Type::NAME,
//!             type_name,
//!             Type::BYTE_OFFSET);
//!     }
//! }
//! impl StructDescriptorVisitor for DescriptorPrinter {
//!     type Output = ()
//!     fn visit_struct<Type>(&self)
//!         -> Self::Output
//!         where Type : StructDescriptor
//!     {
//!         println!("struct {}, with {} fields:",
//!             Type::NAME,
//!             Type::FIELD_COUNT);
//!         // now, introspect over the fields of this type.
//!         ( introwospect_over(Type::Type, Type::Fields, self) );
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
//!      */
//! }
//! ```
//!
//! Because calling the right visitor method is often annoying to
//! do with various different types or in generic contexts, a
//! different form of the `introwospect` keyword can be used to
//! do the exact same thing as above:
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
//!         println!("\t{} ({}, {})\n"
//!             Type::NAME,
//!             type_name,
//!             Type::BYTE_OFFSET);
//!     }
//! }
//! impl StructDescriptorVisitor for DescriptorPrinter {
//!     type Output = ()
//!     fn visit_struct<Type>(&self)
//!         -> Self::Output
//!         where Type : StructDescriptor
//!     {
//!         println!("struct {}, with {} fields:\n",
//!             Type::NAME,
//!             Type::FIELD_COUNT);
//!         // now, introspect over the fields of this type.
//!         ( introwospect_over(Type::Type, Type::Fields, self) );
//!     }
//! }
//!
//! fn main () {
//!     let printer = DescriptorPrinter;
//!     printer.visit_struct(introwospect(other_crate::Meow))
//!     // // Equivalent to this:
//!     //
//!     // introwospect(other_crate::Meow, printer);
//!     //
//!     // // or, equivalent to this:
//!     //
//!     // type MeowInfo = introwospect_type<other_crate::Meow>;
//!     // printer.visit_struct::<MeowInfo>();
//!     // and it should display:
//!     /* struct other_create::Meow with 2 fields:
//!      *     purr_level (i32, 0)
//!      *     scratch_couch (std::time::SystemTime, 8)
//!      */
//! }
//! ```
//!
//! Not all the time, however, is it beneficial to keep all of
//! this information at compile-time. So, this library offers a
//! visitor to convert everything to its more generic and
//! readily-usable form of descriptors that are type-erased,
//! employing several different kinds of type erasure and
//! possibly allocation:
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
//!     let printer = DescriptorPrinter;
//!     let any_struct: AnyStructDescriptor
//!         = introwospect(other_crate::Meow, ToAnyDescriptorVisitor);
//!     /* use type-erased information here. */
//! }
//! ```
//!
//! All of these data types follow the naming convention `Any{}Descriptor`,
//! where `{}` is filled in with `Struct`, `Field`, `Enum`, `Array`, `Tuple`,
//! and so-on, and so-forth.

#![unstable(feature = "introwospection", issue = "none")]

use crate::any::type_name;
use crate::any::{Any, TypeId};
use crate::fmt::{Debug, Display, Formatter, Result};
#[cfg(not(bootstrap))]
use crate::mem::offset_of;
use crate::mem::Discriminant;
use crate::option::Option;

/// The Abstract Data Type (ADT) identifier for determining the kind of ADT being
/// reflected over. Included as part of anything that implements ghe `AdtDescriptor`
/// trait.
#[non_exhaustive]
pub enum AdtId {
    /// An abstract data type made using the `struct` keyword. Offsets are from the beginning
    /// of the object, and may not correspond to the source code order of the field.
    Struct,
    /// An abstract data type made using the `union` keyword. Most
    /// of the offsets for a `union` (or for a `#[repr(C)]` enumeration)
    /// should be at or close to 0, and the discriminant is generally
    /// managed by the user rather than the compiler, which makes it
    /// an unsafe construct.
    Union,
    /// An abstract data type made using the `enum` keyword. It contains 0 or
    /// more variants which are fields directly related to the enumeration itself,
    /// and whose offsets are calculated as from the base of the enumeration,
    /// and NOT as an independent data type.
    Enum,
    /// A tuple, created by a list of initializers or types enclosed by parentheses
    /// (e.g., `( ... )`). Note that the `unit` type is just the empty tuple, and so
    /// is not listed as a separate type in this list.
    Tuple,
    /// An array type, usually formed through initializer with square brackets. The type is
    /// a type parameter `T` and a const generic parameter `const N: usize`, e.g. `[None; N]`.
    Array,
    /// A slice type, usually not directly instantiated but instead borrowed as a view
    /// over another type like `[T; N]` or `Vec<T>`, typically via slice indexing,
    /// e.g. `[0; 100][13..=37]`.
    Slice,
    /// A function definition, such as those defined with the `fn` keyword.
    Function,
}

/// An empty `struct` whose sole job is to indicate when a variant is unspecified.
/// This is important in the case of two variants of e.g. an
/// enumeration which only differ by the use of nothing and the
/// use of () in the variants. That is:
/// ````
/// enum E0 { A }
/// enum E1 { A() }
/// ````
/// are two entirely separate constructs with different meanings. There
/// is no in-language type description for the `E0::A`, while `E1::A`'s
/// "field" is just the unit type `()`.
///
/// `struct`s such as
/// ```
/// struct S0;
/// struct S1();
/// struct S1{};
/// ```
/// may also, apparently, have subtle differences, but this is indicated
/// elsewhere in the API by field counts and whether or not those fields
/// are anonymous (their `NAME` is empty) or have names.
pub struct NoType;

/// An instance of the `NoType` structure, for use in returning a consistent
/// "not available" representation for specific fields on the creation of the
/// `Any{…}Descriptor` types.
const NO_TYPE: NoType = NoType;

/// An enumeration for determining the different ways a struct or variant
/// is defined.
#[non_exhaustive]
pub enum FieldSyntax {
    /// Nothing was used, as in e.g. E::A in `enum E { A }`
    /// or `struct S;`.
    Nothing,
    /// Parentheses were used, as in e.g. E::A in `enum E { A() }`
    /// or `struct S()`.
    Parentheses,
    /// Braces were used, as in e.g. E::A in `enum E { A{} }`
    /// or `struct S{}`.
    Braces,
}

/// A list of names and values of attributes contained within the
/// `#[introwospection(...)]` attribute.
#[non_exhaustive]
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
pub unsafe trait AdtDescriptor {
    /// The identifying ID of an abstract data type, for matching on whether it's a function,
    /// `struct`, `union`, and similar useful identifications.
    const ID: AdtId;
    /// The name of the abstract data type. This is meant to be the full path of the data type,
    /// according to the version of Rust this was compiled against.
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

/// A description for an ADT which may be associated with an inherent implementation,
/// such as `struct`s, `union`s, and enumerations.
pub unsafe trait InherentFunctionsAdt {
    /// A type describing all of the inherent functions associated with this enumeration.
    ///
    /// NOTE
    /// TODO(thephd) Enable a succint way to describe all of the constraints on this type:
    ///
    /// ```
    /// type InherentFunctions :
    ///     (for <const I: usize = 0..Self::INHERENT_FUNCTION_COUNT>
    ///         InherentFunctionDescriptor<I>
    ///     )
    /// = NoType;
    /// ```
    /// doing this would allow it to be acted upon in a meaningful fashion by generic code,
    /// but such bounds/constraint technology does not exist yet.
    type InherentFunctions = NoType;
    /// The number of inherent functions for this enumeration.
    const INHERENT_FUNCTION_COUNT: usize = 0;
}

/// A description for an ADT which may have fields as part of its creation and description,
/// such as `struct`s, `union`s, tuples, and the variants of enumerations.
pub unsafe trait FieldsAdt {
    /// A type describing all of the fields associated with this type.
    ///
    /// NOTE
    /// TODO(thephd) Enable a succint way to describe all of the constraints on this type:
    ///
    /// ```
    /// type Fields :
    ///     (for <const I: usize = 0..Self::FIELD_COUNT> FieldDescriptor<I>)
    /// = NoType;
    /// ```
    /// doing this would allow it to be acted upon in a meaningful fashion by generic code,
    /// but such bounds/constraint technology does not exist yet.
    type Fields = NoType;
    /// The number of fields on this type.
    const FIELD_COUNT: usize = 0;
    /// What kind of syntax was used to encapsulate the fields on this type.
    const FIELD_SYNTAX: FieldSyntax = FieldSyntax::Nothing;
    /// Whether or not there are any fields which are not visible for this type.
    const NON_VISIBLE_FIELDS: bool = false;
}

/// A description of a `struct` type.
pub unsafe trait StructDescriptor: AdtDescriptor + FieldsAdt {
    /// The type of the `struct` that was described.
    type Type;
}

/// A description of a `union` type.
pub unsafe trait UnionDescriptor: AdtDescriptor + FieldsAdt {
    /// The type of the `union` that was described.
    type Type;
}

/// A description of an enumeration type.
pub unsafe trait EnumDescriptor: AdtDescriptor {
    /// The type of the `enum` that was described.
    type Type;
    /// A type describing all of the variants of this enumeration.
    ///
    /// NOTE
    /// TODO(thephd) Enable a succint way to describe all of the constraints on this type:
    ///
    /// ```
    /// type Variants :
    ///     (for <const I: usize = 0..Self::VARIANT_COUNT> VariantDescriptor<I>)
    /// = NoType;
    /// ```
    /// doing this would allow it to be acted upon in a meaningful fashion by generic code,
    /// but such bounds/constraint technology does not exist yet.
    type Variants = NoType;
    /// The number of variants for this enumeration.
    const VARIANT_COUNT: usize = 0;
    /// Whether or not this enumeration is an integer-style
    /// (`#[repr(IntType)]`) enumeration.
    const IS_INTEGER_ENUMERATION: bool = false;
}

/// A description of a function definition or similar construct.
pub unsafe trait FunctionDescriptor: AdtDescriptor {
    /// The type of the function that was described.
    type Type;
    /// A type describing all of the parameters of this function. If this is `NoType`, then
    /// there were no parameters that were part of this function.
    ///
    /// NOTE
    /// TODO(thephd) Enable a succinct way to describe all of the constraints on this type:
    /// ```
    /// type Parameters :
    ///     (for <const I: usize = 0..Self::PARAMETER_COUNT> ParameterDescriptor<I>)
    /// = NoType;
    /// ```
    /// to specify the proper boundaries to make this type usable in generic contexts. (This is
    /// bikeshed syntax and subject to change, as there is already a `for <T>` trait bounds
    /// feature in Rust.)
    type Parameters = NoType;
    /// The return type of of the function.
    type ReturnType;
    /// The number of parameters in the function. Note that a pattern constitutes a
    /// single parameter.
    const PARAMETER_COUNT: usize = 0;
}

/// A description of a function implementation on an existing type as part of its `impl`
/// definition. Note that this does not include implementations of a specific trait for
/// a specific type.
pub unsafe trait InherentFunctionDescriptor<const INDEX: usize>: FunctionDescriptor {
    /// The type this inherent function belongs to. Note that this does not imply a
    /// `self` parameter or `Self` return exists on the function.
    type Owner;
}

/// A description of a built-in array type.
pub unsafe trait ArrayDescriptor: AdtDescriptor {
    /// The full type of the array.
    type Type;
    /// The element type of the array.
    type Element;
    /// The number of elements of type `Element` in this array.
    const ELEMENT_COUNT: usize = 0;
}

/// A description of a built-in slice type.
pub unsafe trait SliceDescriptor: AdtDescriptor {
    /// The full type of the slice.
    type Type;
    /// The element type of the slice.
    type Element;
}

/// A description of a built-in tuple type.
pub unsafe trait TupleDescriptor: AdtDescriptor + FieldsAdt {
    /// The full type of the tuple.
    type Type;
}

/// A parameter for a function definition, or similar construction.
pub unsafe trait ParameterDescriptor<const PARAMETER_INDEX: usize> {
    /// The function type related to this parameter descriptor.
    type Owner;
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
/// of an enumeration, a field on a `struct`, or a field on a `union`.
///
/// `DECLARATION_INDEX` is the 0-based index of the field in declaration (source code) order.
pub unsafe trait FieldDescriptor<const DECLARATION_INDEX: usize> {
    /// The type that owns this field. It may be any abstract data type, such as a `union`, a variant on an `enum`,
    /// a tuple, or a `struct` type. All (byte) offsets are from the base of an `Self::Owner` object.
    type Owner;
    /// The data type of the field itself.
    type Type;
    /// The 0-based declaration (source code) index.
    const DECLARATION_INDEX: usize = DECLARATION_INDEX;
    /// The name of the field within the `union`, variant, tuple, or `struct`. If this is empty, it
    /// signifies a completely unnamed field. If this is part of a tuple-like field syntax,
    /// then the name of the field will not be empty, but instead be `0` or similar.
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
/// `DECLARATION_INDEX` is the index of the variant in declaration (source code) order.
pub unsafe trait VariantDescriptor<const DECLARATION_INDEX: usize>: FieldsAdt {
    /// The type which owns this variant.
    type Owner: 'static + EnumDescriptor;
    /// The integer type that is used for this declaration if it was declared with the representation
    /// attribute, `#[repr(Int)]`. Used in conjunction with the `INTEGER_VALUE` associated
    /// `const` item.
    type Int: 'static = NoType;
    /// The 0-based index of the variant in declaration (source code) order.
    const DECLARATION_INDEX: usize = DECLARATION_INDEX;
    /// The name of the variant within the enumeration.
    const NAME: &'static str;
    /// The discriminant that identifies this variant of the data type. The discriminant can
    /// be used when looping over all fields to find which variant of an enumeration is the
    /// currently active variant. Then, the `Fields` or `INTEGER_VALUE` -- if present --
    /// can be used to deduce the fields at the specific offset from an object of the enumeration
    /// type, or can be used to get the constant integer value of this variant in the enumeration,
    /// respectively.
    ///
    /// NOTE(
    /// TODO(thephd)) Enable supporting the intrinsic:
    /// ```
    /// const DISCRIMINANT : &'static Discriminant<Self::Owner> =
    ///      &std::mem::discriminant_at<Self::Owner>(Self::DECLARATION_INDEX);
    /// ```
    /// to get a discriminant at compile-time without needing to generate a fake
    /// enumeration object.
    const DISCRIMINANT: &'static Discriminant<Self::Owner>;
    /// The size of this variant within the enumeration type.
    const BYTE_SIZE: usize = 0;
    /// The value of an enumeration which opts into a `#[repr(Int)]` representation.
    /// If the enumeration has not opted into such a representation, then this will be
    /// `None`. Otherwise, `Self::Int` will be set to the integer type specified in the
    /// representation attribute and the value of the enumeration will be stored here.
    const INTEGER_VALUE: Option<&'static Self::Int> = None;
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
#[const_trait]
pub trait TupleDescriptorVisitor {
    /// The return type of the `visit_tuple` and `visit_tuple_mut` implementations.
    type Output;

    /// A visitation function for a specific `TupleDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_tuple<Type: 'static>(&self) -> Self::Output
    where
        Type: TupleDescriptor;

    /// A visitation function for a specific `TupleDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_tuple::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_tuple_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: TupleDescriptor,
    {
        return Self::visit_tuple::<Type>(&self);
    }
}

/// A visitor on a `SliceDescriptor` trait implementation, to handle the compile-time
/// data stored on such a trait implementation.
#[const_trait]
pub trait SliceDescriptorVisitor {
    /// The return type of the `visit_slice` and `visit_slice_mut` implementations.
    type Output;

    /// A visitation function for a specific `SliceDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_slice<Type: 'static>(&self) -> Self::Output
    where
        Type: SliceDescriptor;

    /// A visitation function for a specific `SliceDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_slice::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_slice_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: SliceDescriptor,
    {
        return Self::visit_slice::<Type>(&self);
    }
}

/// A visitor on a `ArrayDescriptor` trait implementation, to handle the compile-time
/// data stored on such a trait implementation.
#[const_trait]
pub trait ArrayDescriptorVisitor {
    /// The return type of the `visit_array` and `visit_array_mut` implementations.
    type Output;

    /// A visitation function for a specific `ArrayDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_array<Type: 'static>(&self) -> Self::Output
    where
        Type: ArrayDescriptor;

    /// A visitation function for a specific `ArrayDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_array::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_array_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: ArrayDescriptor,
    {
        return Self::visit_array::<Type>(&self);
    }
}

/// A visitor on a `ArrayDescriptor` trait implementation, to handle the compile-time
/// data stored on such a trait implementation.
#[const_trait]
pub trait StructDescriptorVisitor {
    /// The return type of the `visit_struct` and `visit_struct_mut` implementations.
    type Output;

    /// A visitation function for a specific `StructDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_struct<Type: 'static>(&self) -> Self::Output
    where
        Type: StructDescriptor;

    /// A visitation function for a specific `StructDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_struct::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_struct_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: StructDescriptor,
    {
        return Self::visit_struct::<Type>(&self);
    }
}

/// A visitor on a `UnionDescriptor` trait implementation, to handle the compile-time
/// data stored on such a trait implementation.
#[const_trait]
pub trait UnionDescriptorVisitor {
    /// The return type of the `visit_union` and `visit_union_mut` implementations.
    type Output;

    /// A visitation function for a specific `UnionDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_union<Type: 'static>(&self) -> Self::Output
    where
        Type: UnionDescriptor;

    /// A visitation function for a specific `UnionDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_union::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_union_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: UnionDescriptor,
    {
        return Self::visit_union::<Type>(&self);
    }
}

/// A visitor on a `EnumDescriptor` trait implementation, to handle the compile-time
/// data stored on such a trait implementation.
#[const_trait]
pub trait EnumDescriptorVisitor {
    /// The return type of the `visit_enum` and `visit_enum_mut` implementations.
    type Output;

    /// A visitation function for a specific `EnumDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_enum<Type: 'static>(&self) -> Self::Output
    where
        Type: EnumDescriptor;

    /// A visitation function for a specific `EnumDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_enum::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_enum_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: EnumDescriptor,
    {
        return Self::visit_enum::<Type>(&self);
    }
}

/// A visitor on a `FunctionDescriptor` trait implementation, to handle the compile-time
/// data stored on such a trait implementation.
#[const_trait]
pub trait FunctionDescriptorVisitor {
    /// The return type of the `visit_function` and `visit_function_mut` implementations.
    type Output;

    /// A visitation function for a specific `FunctionDescriptor` type. This form
    /// is immutable, and so cannot modify its `self` argument.
    ///
    /// Returns `Self::Output`
    fn visit_function<Type: 'static>(&self) -> Self::Output
    where
        Type: FunctionDescriptor;

    /// A visitation function for a specific `FunctionDescriptor` type. This form
    /// is mutable, and by default calls `Self::visit_function::<Type>(&self)`.
    ///
    /// Returns `Self::Output`
    fn visit_function_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: FunctionDescriptor,
    {
        return Self::visit_function::<Type>(&self);
    }
}

/// A visitor for a collection of parameter descriptors that handles all incoming compile-time
/// data stored on an implementation of a typical `ParameterDescriptor<I>`, where `I` is from `0`
/// to the maximum parameter count of a given type. This is used for function types.
#[const_trait]
pub trait ParameterDescriptorVisitor {
    /// The return type of the `visit_parameter` and `visit_parameter_mut` implementations.
    type Output;

    /// A visitation function for a specific `ParameterDescriptor` type.
    ///
    /// Returns `Self::Output`.
    fn visit_parameter<Type: 'static, const DECLARATION_INDEX: usize>(&self) -> Self::Output
    where
        Type: ParameterDescriptor<DECLARATION_INDEX>;

    /// A visitation function for a specific `ParameterDescriptor` type. This form is mutable,
    /// and by default calls `Self::visit_parameter::<Type, DECLARATION_INDEX>`.
    ///
    /// Returns `Self::Output`
    fn visit_parameter_mut<Type: 'static, const DECLARATION_INDEX: usize>(&mut self) -> Self::Output
    where
        Type: ParameterDescriptor<DECLARATION_INDEX>,
    {
        return Self::visit_parameter::<Type, DECLARATION_INDEX>(&self);
    }
}

/// A visitor for a collection of parameter descriptors that handles all incoming compile-time
/// data stored on an implementation of a typical `ParameterDescriptor<I>`, where `I` is from `0`
/// to the maximum parameter count of a given type.
///
/// In this version, the `DECLARATION_INDEX` is provided on the visitor itself, which allows for
/// each different parameter handled by this visitor to have a different `type Output` on its
/// implementation.
#[const_trait]
pub trait ParameterDescriptorVisitorAt<const DECLARATION_INDEX: usize> {
    /// The return type of the `visit_parameter` and `visit_parameter_mut` implementations.
    type Output;

    /// A visitation function for a specific `ParameterDescriptor` type.
    ///
    /// Returns `Self::Output`.
    fn visit_parameter<Type: 'static>(&self) -> Self::Output
    where
        Type: ParameterDescriptor<DECLARATION_INDEX>;

    /// A visitation function for a specific `ParameterDescriptor` type. This form is mutable,
    /// and by default calls `Self::visit_parameter::<Type, DECLARATION_INDEX>`.
    ///
    /// Returns `Self::Output`
    fn visit_parameter_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: ParameterDescriptor<DECLARATION_INDEX>,
    {
        return Self::visit_parameter::<Type>(&self);
    }
}

/// A visitor for a collection of field descriptors that handles all incoming compile-time
/// data stored on an implementation of a typical `FieldDescriptor<I>`, where `I` is from `0`
/// to the maximum field count of a given type. This is used for `struct` types, `union` types,
/// `enum` variants, and tuple types.
#[const_trait]
pub trait FieldDescriptorVisitor {
    /// The return type of the `visit_field` and `visit_field_mut` implementations.
    type Output;

    /// A visitation function for a specific `FieldDescriptor<DECLARATION_INDEX>`
    /// implementation.
    ///
    /// Returns `Self::Output`.
    fn visit_field<Type: 'static, const DECLARATION_INDEX: usize>(&self) -> Self::Output
    where
        Type: FieldDescriptor<DECLARATION_INDEX>;

    /// A visitation function for a specific `FieldDescriptor<DECLARATION_INDEX>` type. This
    /// form is mutable, and by default calls `Self::visit_field::<Type, DECLARATION_INDEX>`.
    ///
    /// Returns `Self::Output`.
    fn visit_field_mut<Type: 'static, const DECLARATION_INDEX: usize>(&mut self) -> Self::Output
    where
        Type: FieldDescriptor<DECLARATION_INDEX>,
    {
        return Self::visit_field::<Type, DECLARATION_INDEX>(&self);
    }
}

/// A visitor for a collection of field descriptors that handles all incoming compile-time
/// data stored on an implementation of a typical `FieldDescriptor<I>`, where `I` is from `0`
/// to the maximum field count of a given type. This is used for `struct` and `union` types.
///
/// Providing the `DECLARATION_INDEX` as part of the visitor allows for each different parameter
/// handled by this visitor to have a different `type Output` on its implementation. This version
/// of the trait is preferred over a `FieldDescriptorVisitor` if it exists for this particular
/// `DECLARATION_INDEX` on the visitor.

#[const_trait]
pub trait FieldDescriptorVisitorAt<const DECLARATION_INDEX: usize> {
    /// The return type of the `visit_field` and `visit_field_mut` implementations.
    type Output;

    /// A visitation function for a specific `FieldDescriptor<DECLARATION_INDEX>`
    /// implementation.
    ///
    /// Returns `Self::Output`.
    fn visit_field<Type: 'static>(&self) -> Self::Output
    where
        Type: FieldDescriptor<DECLARATION_INDEX>;

    /// A visitation function for a specific `FieldDescriptor<DECLARATION_INDEX>` type. This
    /// form is mutable, and by default calls `Self::visit_field::<Type>`.
    ///
    /// Returns `Self::Output`.
    fn visit_field_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: FieldDescriptor<DECLARATION_INDEX>,
    {
        return Self::visit_field::<Type>(&self);
    }
}

/// A visitor for a collection of variant descriptors that handles all incoming compile-time
/// data stored on an implementation of a typical `VariantDescriptor<I>`, where `I` is from `0`
/// to the maximum variant count of a given type. This is used for enumeration types.
#[const_trait]
pub trait VariantDescriptorVisitor {
    /// The return type of the `visit_function` and `visit_function_mut` implementations.
    type Output;

    /// A visitation function for a specific `VariantDescriptor<DECLARATION_INDEX>`
    /// implementation.
    ///
    /// Returns `Self::Output`.
    fn visit_variant<Type: 'static, const DECLARATION_INDEX: usize>(&self) -> Self::Output
    where
        Type: VariantDescriptor<DECLARATION_INDEX>;

    /// A visitation function for a specific `VariantDescriptor<DECLARATION_INDEX>` type.
    /// This form is mutable, and by default calls
    /// `Self::visit_variant::<Type, DECLARATION_INDEX>`.
    ///
    /// Returns `Self::Output`.
    fn visit_variant_mut<Type: 'static, const DECLARATION_INDEX: usize>(&mut self) -> Self::Output
    where
        Type: VariantDescriptor<DECLARATION_INDEX>,
    {
        return Self::visit_variant::<Type, DECLARATION_INDEX>(&self);
    }
}

/// A visitor for a collection of variant descriptors that handles all incoming compile-time
/// data stored on an implementation of a typical `VariantDescriptor<I>`, where `I` is from `0`
/// to the maximum variant count of a given type. This is used for enumeration types, and is
/// preferred over the `VariantDescriptorVisitor` if one exists for this for particular
/// `DECLARATION_INDEX`.
#[const_trait]
pub trait VariantDescriptorVisitorAt<const DECLARATION_INDEX: usize> {
    /// The return type of the `visit_function` and `visit_function_mut` implementations.
    type Output;

    /// A visitation function for a specific `VariantDescriptor<DECLARATION_INDEX>`
    /// implementation.
    ///
    /// Returns `Self::Output`.
    fn visit_variant<Type: 'static>(&self) -> Self::Output
    where
        Type: VariantDescriptor<DECLARATION_INDEX>;

    /// A visitation function for a specific `VariantDescriptor<DECLARATION_INDEX>` type.
    /// This form is mutable, and by default calls
    /// `Self::visit_variant::<Type, DECLARATION_INDEX>`.
    ///
    /// Returns `Self::Output`.
    fn visit_variant_mut<Type: 'static>(&mut self) -> Self::Output
    where
        Type: VariantDescriptor<DECLARATION_INDEX>,
    {
        return Self::visit_variant::<Type>(&self);
    }
}

/// A run-time description of a `struct` type.
#[derive(Debug)]
#[non_exhaustive]
pub struct AnyStructDescriptor {
    /// The type of this `struct`.
    pub adt_id: AdtId,
    /// The type of the `struct` that was described.
    pub type_id: TypeId,
    /// The name of the `struct`.
    pub name: &'static str,
    /// Whether or not this `struct` is a tuple `struct` (it has the form
    /// `struct SomeStruct(T0, ..., TN)`).
    pub field_syntax: FieldSyntax,
    /// A slice describing each field of this `struct` type.
    pub fields: &'static [AnyFieldDescriptor],
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

/// A run-time description of a `union` type.
#[derive(Debug)]
#[non_exhaustive]
pub struct AnyUnionDescriptor {
    /// The type of this `union`.
    pub adt_id: AdtId,
    /// The type of the `union` that was described.
    pub type_id: TypeId,
    /// The name of the `union`.
    pub name: &'static str,
    /// What kind of syntax was used to encapsulate the fields for this `union`'s fields.
    pub field_syntax: FieldSyntax,
    /// A slice describing each field of this `union` type.
    pub fields: &'static [AnyFieldDescriptor],
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

/// A run-time description of an enumeration type.
#[derive(Debug)]
#[non_exhaustive]
pub struct AnyEnumDescriptor {
    /// The type of the `enum` that was described.
    pub type_id: TypeId,
    /// The name of this `enum`.
    pub name: &'static str,
    /// A slice describing each variant of this `enum` type.
    pub variants: &'static [AnyVariantDescriptor],
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

/// A run-time description of an enumeration type.
#[derive(Debug)]
#[non_exhaustive]
pub struct AnySliceDescriptor {
    /// The type of the slice that was described.
    pub type_id: TypeId,
    /// The name of the slice.
    pub name: &'static str,
    /// The type of each element.
    pub element_type_id: TypeId,
    /// A built-in type has no user-controllable attributes, but this is here either way.
    pub attributes: &'static [AttributeDescriptor; 0],
}

/// A run-time description of an enumeration type.
#[derive(Debug)]
#[non_exhaustive]
pub struct AnyArrayDescriptor {
    /// The type of the `enum` that was described.
    pub type_id: TypeId,
    /// The name of the enumeration.
    pub name: &'static str,
    /// The type of each element.
    pub element_type_id: TypeId,
    /// The number of compile-time elements in the array.
    pub element_count: usize,
    /// A built-in type has no user-controllable attributes, but this is here either way.
    pub attributes: &'static [AttributeDescriptor; 0],
}

/// A run-time description of a tuple type.
#[derive(Debug)]
#[non_exhaustive]
pub struct AnyTupleDescriptor {
    /// The type of the tuple that was described.
    pub type_id: TypeId,
    /// The name of the tuple. For tuples, this will just be the
    /// amalgamation of each field's type names enclosed in parentheses.
    pub name: &'static str,
    /// A slice describing each field of this tuple. If this is empty,
    /// this is the unit type.
    pub fields: &'static [AnyFieldDescriptor],
    /// The field syntax for this type. Should always be `FieldSyntax::Parentheses`.
    pub field_syntax: FieldSyntax,
    /// A built-in type has no user-controllable attributes, but this is here either way.
    pub attributes: &'static [AttributeDescriptor; 0],
}

/// A run-time description of a function definition, or similar construct.
#[derive(Debug)]
#[non_exhaustive]
pub struct AnyFunctionDescriptor {
    /// The name of the function definition, or similar construct.
    pub name: &'static str,
    /// The return type of this function.
    pub return_type_id: TypeId,
    /// A list of parameters that make up this function call.
    pub parameters: &'static [AnyParameterDescriptor],
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

/// A run-time description of a possible data type.
#[derive(Debug)]
#[non_exhaustive]
pub enum AnyAdtDescriptor {
    /// A `struct` type.
    Struct(AnyStructDescriptor),
    /// A `union` type.
    Union(AnyUnionDescriptor),
    /// An enumeration type.
    Enum(AnyEnumDescriptor),
    /// An array type.
    Array(AnyArrayDescriptor),
    /// A slice type.
    Slice(AnySliceDescriptor),
    /// A tuple type.
    Tuple(AnyTupleDescriptor),
    /// A function type.
    Function(AnyFunctionDescriptor),
}

/// A parameter in a function declaration or similar construct.
#[derive(Debug)]
#[non_exhaustive]
pub struct AnyParameterDescriptor {
    /// A description of the function tyoe that is related to this parameter.
    pub owner_type_id: AnyFunctionDescriptor,
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
/// of an enumeration, a field on a `struct` type, a field on a `union` type, or a field on
/// a `struct` type.
#[derive(Debug)]
#[non_exhaustive]
pub struct AnyFieldDescriptor {
    /// The type that owns this field. It may be any abstract data type such as a `union`, a
    /// `enum`'s variant, a tuple, or a `struct` type. All (byte) offsets are from the base
    /// of an `Self::Owner` object.
    pub owner_type_id: TypeId,
    /// The data type of the field itself.
    pub type_id: TypeId,
    /// The 0-based declaration (source code) index.
    pub declaration_index: usize,
    /// The name of the field within the `union`, `enum`'s variant, tuple or `struct` type. If
    /// this is empty, it signifies an completely unnamed field. If this is part of a tuple-like
    /// field syntax, then the name of the field will not be empty, but instead be `0` or similar.
    pub name: &'static str,
    /// The byte offset from the base of an owner type object to the data type of this field.
    pub byte_offset: usize,
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
#[non_exhaustive]
pub struct AnyVariantDescriptor {
    /// The enumeration that owns this variant.
    pub owner_type_id: TypeId,
    /// The 0-based index of the variant in declaration (source code) order.
    pub declaration_index: usize,
    /// The name of the variant within the enumeration.
    pub name: &'static str,
    /// The fields that are part of this variant.
    pub fields: &'static [AnyFieldDescriptor],
    /// Whether or not this variant is a tuple variant (it has the form `A(T0, ..., TN)`).
    pub field_syntax: FieldSyntax,
    /// A type-erased reference to a `Discriminant<T>` type. A user should check the `owner_type_id`
    /// parameter to verify the proper type `T` for the containing data type, then cast this to
    /// the appropriate `Discriminant<T>` to use.
    pub discriminant: &'static dyn Any,
    /// A representation of any possible constant integer value of an enumeration which opts
    /// into a `#[repr(Int)]` representation. May refer to `core::introwospection::NoType`.
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

/// Returns a field of the given data type, offset from the owner type. The returned field will
/// be a non-mutable reference, just like the owner type.
pub const fn get_field<Type, const DECLARATION_INDEX: usize>(owner: &Type::Owner) -> &Type::Type
where
    Type: FieldDescriptor<DECLARATION_INDEX>,
{
    unsafe {
        ((owner as *const Type::Owner as *const u8).add(Type::BYTE_OFFSET).cast::<Type::Type>())
            .as_ref()
            .unwrap_unchecked()
    }
}

/// Returns a field of the given data type, offset from the owner type. The returned field will
/// be a mutable reference, just like the owner type.
pub const fn get_field_mut<Type, const DECLARATION_INDEX: usize>(
    owner: &mut Type::Owner,
) -> &mut Type::Type
where
    Type: FieldDescriptor<DECLARATION_INDEX>,
{
    unsafe {
        ((owner as *mut Type::Owner as *mut u8).add(Type::BYTE_OFFSET).cast::<Type::Type>())
            .as_mut()
            .unwrap_unchecked()
    }
}

/// Returns a field of the given data type, offset from the owner type, if the
/// provided `Field` and `Owner` types matches what is stored in the `AnyFieldDescriptor`.
/// The return field will be a non-mutable reference, just like the owner type.
pub fn get_any_field<'owner_lifetime, 'field_lifetime, Field: 'static, Owner: 'static>(
    field_descriptor: &'field_lifetime AnyFieldDescriptor,
    owner: &'owner_lifetime Owner,
) -> Option<&'owner_lifetime Field> {
    if field_descriptor.type_id != TypeId::of::<Field>()
        || field_descriptor.owner_type_id != TypeId::of::<Owner>()
    {
        return None;
    }
    unsafe {
        Some(
            ((owner as *const Owner as *const u8)
                .add(field_descriptor.byte_offset)
                .cast::<Field>())
            .as_ref()
            .unwrap_unchecked(),
        )
    }
}

/// Returns a field of the given data type, offset from the owner type, if the
/// provided `Field` and `Owner` types matches what is stored in the `AnyFieldDescriptor`.
/// The returned field will be a mutable reference, just like the owner type.
pub fn get_any_field_mut<'owner_lifetime, 'field_lifetime, Field: 'static, Owner: 'static>(
    field_descriptor: &'field_lifetime AnyFieldDescriptor,
    owner: &'owner_lifetime mut Owner,
) -> Option<&'owner_lifetime mut Field> {
    if field_descriptor.type_id != TypeId::of::<Field>()
        || field_descriptor.owner_type_id != TypeId::of::<Owner>()
    {
        return None;
    }
    unsafe {
        Some(
            ((owner as *mut Owner as *mut u8).add(field_descriptor.byte_offset).cast::<Field>())
                .as_mut()
                .unwrap_unchecked(),
        )
    }
}

/// A function which takes 2 byte slices and compares them for strict equality, in both values and size.
pub const fn u8_slice_equals(x: &[u8], y: &[u8]) -> bool {
    if x.len() != y.len() {
        return false;
    }

    let mut first = x;
    let mut second = y;
    while let ([h1, tail1 @ ..], [h2, tail2 @ ..]) = (first, second) {
        if *h1 != *h2 {
            return false;
        }

        first = tail1;
        second = tail2;
    }

    true
}

/// A function which takes 2 string slices and compares them for strict equality, in both values and size.
pub const fn str_equals(x: &str, y: &str) -> bool {
    u8_slice_equals(x.as_bytes(), y.as_bytes())
}

/// Finds the attribute with the given name.
pub const fn find_attribute<'attributes_lifetime>(
    name: &str,
    attributes: &'attributes_lifetime [AttributeDescriptor],
) -> Option<&'attributes_lifetime AttributeDescriptor> {
    let attributes_max_len: usize = attributes.len();
    let mut attributes_index: usize = 0;
    while attributes_index < attributes_max_len {
        let attribute = &attributes[attributes_index];
        attributes_index += 1;
        if str_equals(name, attribute.name) {
            return Some(attribute);
        }
    }
    None
}

/// Checks if a given attribute with the specified value exists within the attribute collection.
///
/// The value must match exactly. That is, if `None` is used for `value` and it is
pub const fn contains_attribute_with_value(
    name: &str,
    value: Option<&str>,
    attributes: &[AttributeDescriptor],
) -> bool {
    let maybe_attribute = find_attribute(name, attributes);
    if maybe_attribute.is_none() {
        return false;
    }
    let attribute = maybe_attribute.unwrap();
    if attribute.value.is_none() {
        return value.is_none();
    }
    value.is_some() && str_equals(attribute.value.unwrap(), value.unwrap())
}

/// Checks if a given attribute with the specified value exists within the attribute collection.
pub const fn contains_attribute(name: &str, attributes: &[AttributeDescriptor]) -> bool {
    let maybe_attribute = find_attribute(name, attributes);
    maybe_attribute.is_some()
}

/// A visitor for taking an existing `struct`, `union`, `enum`, `enum`'s variant, tuple, array, slice, or
/// other Rust abstract data type and producing a concrete, type-erased `Any`-style variant of it. See
/// `AnyStructDescriptor`, `AnyUnionDescriptor`, `AnyTupleDescriptor`, `AnySliceDescriptor`,
/// `AnyArrayDescriptor`, `AnyFieldDescriptor`, and `AnyVariantDescriptor` for more information.
#[derive(Debug)]
#[non_exhaustive]
pub struct ToAnyDescriptorVisitor;

impl StructDescriptorVisitor for ToAnyDescriptorVisitor {
    type Output = AnyStructDescriptor;

    fn visit_struct<Type: 'static>(&self) -> Self::Output
    where
        Type: StructDescriptor,
    {
        let fields: &'static [AnyFieldDescriptor] = &[];
        // TODO(thephd):
        // let fields : &'static [AnyFieldDescriptor]
        //     = &[introwospect_over(Type::Type, Type::Fields, &self)];
        AnyStructDescriptor {
            adt_id: Type::ID,
            name: Type::NAME,
            type_id: TypeId::of::<Type::Type>(),
            field_syntax: Type::FIELD_SYNTAX,
            fields,
            attributes: Type::ATTRIBUTES,
        }
    }
}

impl UnionDescriptorVisitor for ToAnyDescriptorVisitor {
    type Output = AnyUnionDescriptor;

    fn visit_union<Type: 'static>(&self) -> Self::Output
    where
        Type: UnionDescriptor,
    {
        let fields: &'static [AnyFieldDescriptor] = &[];
        // TODO(thephd):
        // let fields : &'static [AnyFieldDescriptor]
        //     = &[introwospect_over(Type::Type, Type::Fields, &self)];
        AnyUnionDescriptor {
            adt_id: Type::ID,
            field_syntax: Type::FIELD_SYNTAX,
            name: Type::NAME,
            type_id: TypeId::of::<Type::Type>(),
            fields,
            attributes: Type::ATTRIBUTES,
        }
    }
}

impl EnumDescriptorVisitor for ToAnyDescriptorVisitor {
    type Output = AnyEnumDescriptor;

    fn visit_enum<Type: 'static>(&self) -> Self::Output
    where
        Type: EnumDescriptor,
    {
        let variants: &'static [AnyVariantDescriptor] = &[];
        // TODO(thephd) fix to get compilation of:
        // let fields : &'static [AnyFieldDescriptor]
        //     = &[introwospect_over(Type::Type, Type::Fields, &self)];
        AnyEnumDescriptor {
            name: Type::NAME,
            type_id: TypeId::of::<Type::Type>(),
            variants,
            attributes: Type::ATTRIBUTES,
        }
    }
}

impl FunctionDescriptorVisitor for ToAnyDescriptorVisitor {
    type Output = AnyFunctionDescriptor;

    fn visit_function<Type: 'static>(&self) -> Self::Output
    where
        Type: FunctionDescriptor,
    {
        // TODO(thephd) Allow compilation of:
        // let parameters: &'static [AnyParameterDescription]
        //     = &[introwospect_over(Type::Type, Type::Parameters, self)];
        let parameters: &'static [AnyParameterDescriptor] = &[];
        AnyFunctionDescriptor {
            name: Type::NAME,
            return_type_id: TypeId::of::<Type::ReturnType>(),
            parameters,
            attributes: Type::ATTRIBUTES,
        }
    }
}

impl TupleDescriptorVisitor for ToAnyDescriptorVisitor {
    type Output = AnyTupleDescriptor;

    fn visit_tuple<Type: 'static>(&self) -> Self::Output
    where
        Type: TupleDescriptor,
    {
        // TODO(thephd) Allow compilation of:
        // let fields: &'static [AnyFieldDescription]
        //     = &[introwospect_over(Type::Type, Type::Fields, self)];
        let fields: &'static [AnyFieldDescriptor] = &[];
        AnyTupleDescriptor {
            name: Type::NAME,
            type_id: TypeId::of::<Type>(),
            fields,
            field_syntax: Type::FIELD_SYNTAX,
            attributes: &[],
        }
    }
}

impl SliceDescriptorVisitor for ToAnyDescriptorVisitor {
    type Output = AnySliceDescriptor;

    fn visit_slice<Type: 'static>(&self) -> Self::Output
    where
        Type: SliceDescriptor,
    {
        AnySliceDescriptor {
            name: Type::NAME,
            type_id: TypeId::of::<Type>(),
            element_type_id: TypeId::of::<Type::Element>(),
            attributes: &[],
        }
    }
}

impl ArrayDescriptorVisitor for ToAnyDescriptorVisitor {
    type Output = AnyArrayDescriptor;

    fn visit_array<Type: 'static>(&self) -> Self::Output
    where
        Type: ArrayDescriptor,
    {
        AnyArrayDescriptor {
            name: Type::NAME,
            type_id: TypeId::of::<Type>(),
            element_type_id: TypeId::of::<Type::Element>(),
            element_count: Type::ELEMENT_COUNT,
            attributes: &[],
        }
    }
}

impl FieldDescriptorVisitor for ToAnyDescriptorVisitor {
    type Output = AnyFieldDescriptor;

    fn visit_field<Type: 'static, const DECLARATION_INDEX: usize>(&self) -> Self::Output
    where
        Type: FieldDescriptor<DECLARATION_INDEX>,
    {
        AnyFieldDescriptor {
            name: Type::NAME,
            owner_type_id: TypeId::of::<Type::Owner>(),
            type_id: TypeId::of::<Type>(),
            declaration_index: DECLARATION_INDEX,
            byte_offset: Type::BYTE_OFFSET,
            attributes: Type::ATTRIBUTES,
        }
    }
}

impl VariantDescriptorVisitor for ToAnyDescriptorVisitor {
    type Output = AnyVariantDescriptor;

    fn visit_variant<Type: 'static, const DECLARATION_INDEX: usize>(&self) -> Self::Output
    where
        Type: VariantDescriptor<DECLARATION_INDEX>,
    {
        // TODO(thephd) Enable syntax for iterating over fields
        // let fields: &'static [AnyFieldDescription]
        //     = &[introwospect_over(Type::Owner, Type::Fields, self)];
        let fields: &'static [AnyFieldDescriptor] = &[];
        AnyVariantDescriptor {
            name: Type::NAME,
            owner_type_id: TypeId::of::<Type::Owner>(),
            discriminant: Type::DISCRIMINANT as &'static dyn Any,
            declaration_index: DECLARATION_INDEX,
            field_syntax: Type::FIELD_SYNTAX,
            fields,
            integer_value: match Type::INTEGER_VALUE {
                Some(val) => val as &'static dyn Any,
                None => &NO_TYPE as &'static dyn Any,
            },
            attributes: Type::ATTRIBUTES,
        }
    }
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

impl Debug for FieldSyntax {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(self, f)
    }
}

impl Display for FieldSyntax {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FieldSyntax::Nothing => Display::fmt("core::introwospection::FieldSyntax::Nothing", f),
            FieldSyntax::Parentheses => {
                Display::fmt("core::introwospection::FieldSyntax::Parentheses", f)
            }
            FieldSyntax::Braces => Display::fmt("core::introwospection::FieldSyntax::Nothing", f),
        }
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

// NoType / none indicator type
unsafe impl AdtDescriptor for NoType {
    const ID: AdtId = AdtId::Struct;
    const NAME: &'static str = type_name::<NoType>();
}

unsafe impl StructDescriptor for NoType {
    type Type = NoType;
}
unsafe impl FieldsAdt for NoType {}

// NoType / none indicator type
unsafe impl<'life> AdtDescriptor for &'life NoType {
    const ID: AdtId = AdtId::Struct;
    const NAME: &'static str = type_name::<&'life NoType>();
}

unsafe impl<'life> StructDescriptor for &'life NoType {
    type Type = &'life NoType;
}
unsafe impl<'life> FieldsAdt for &'life NoType {}

// slices
unsafe impl<'slice_lifetime, T> AdtDescriptor for &'slice_lifetime [T] {
    const ID: AdtId = AdtId::Slice;
    const NAME: &'static str = type_name::<[T]>();
}

unsafe impl<'slice_lifetime, T> SliceDescriptor for &'slice_lifetime [T] {
    type Type = &'slice_lifetime [T];
    type Element = T;
}

// array
unsafe impl<'array_lifetime, T, const N: usize> AdtDescriptor for &'array_lifetime [T; N] {
    const ID: AdtId = AdtId::Array;
    const NAME: &'static str = type_name::<[T; N]>();
}

unsafe impl<'array_lifetime, T, const N: usize> ArrayDescriptor for &'array_lifetime [T; N] {
    type Type = &'array_lifetime [T; N];
    type Element = T;
}

unsafe impl<T, const N: usize> AdtDescriptor for [T; N] {
    const ID: AdtId = AdtId::Array;
    const NAME: &'static str = type_name::<[T; N]>();
}

unsafe impl<T, const N: usize> ArrayDescriptor for [T; N] {
    type Type = [T; N];
    type Element = T;
}

// 0-tuple / unit
unsafe impl AdtDescriptor for () {
    const ID: AdtId = AdtId::Tuple;
    const NAME: &'static str = type_name::<()>();
}

unsafe impl TupleDescriptor for () {
    type Type = ();
}
unsafe impl FieldsAdt for () {
    const FIELD_SYNTAX: FieldSyntax = FieldSyntax::Parentheses;
}
unsafe impl<'lifetime> AdtDescriptor for &'lifetime () {
    const ID: AdtId = AdtId::Tuple;
    const NAME: &'static str = type_name::<&'lifetime ()>();
}

unsafe impl<'lifetime> TupleDescriptor for &'lifetime () {
    type Type = &'lifetime ();
}
unsafe impl<'lifetime> FieldsAdt for &'lifetime () {
    const FIELD_SYNTAX: FieldSyntax = FieldSyntax::Parentheses;
}

// 1-tuple
unsafe impl<T0> AdtDescriptor for (T0,) {
    const ID: AdtId = AdtId::Tuple;
    const NAME: &'static str = type_name::<(T0,)>();
}

unsafe impl<T0> FieldDescriptor<0> for (T0,) {
    type Owner = (T0,);
    type Type = T0;
    const NAME: &'static str = "0";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0,), 0);
}

unsafe impl<T0> TupleDescriptor for (T0,) {
    type Type = (T0,);
}

unsafe impl<T0> FieldsAdt for (T0,) {
    type Fields = (T0,);
    const FIELD_COUNT: usize = 1;
    const FIELD_SYNTAX: FieldSyntax = FieldSyntax::Parentheses;
}

// 2-tuple
unsafe impl<T0, T1> AdtDescriptor for (T0, T1) {
    const ID: AdtId = AdtId::Tuple;
    const NAME: &'static str = type_name::<(T0, T1)>();
}

unsafe impl<T0, T1> FieldDescriptor<0> for (T0, T1) {
    type Owner = (T0, T1);
    type Type = T0;
    const NAME: &'static str = "0";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0, T1), 0);
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
}

unsafe impl<T0, T1> FieldDescriptor<1> for (T0, T1) {
    type Owner = (T0, T1);
    type Type = T1;
    const NAME: &'static str = "1";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0, T1), 1);
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
}

unsafe impl<T0, T1> TupleDescriptor for (T0, T1) {
    type Type = (T0, T1);
}

unsafe impl<T0, T1> FieldsAdt for (T0, T1) {
    type Fields = (T0, T1);
    const FIELD_COUNT: usize = 2;
    const FIELD_SYNTAX: FieldSyntax = FieldSyntax::Parentheses;
}

// 3-tuple
unsafe impl<T0, T1, T2> AdtDescriptor for (T0, T1, T2) {
    const ID: AdtId = AdtId::Tuple;
    const NAME: &'static str = type_name::<(T0, T1, T2)>();
}

unsafe impl<T0, T1, T2> FieldDescriptor<0> for (T0, T1, T2) {
    type Owner = (T0, T1, T2);
    type Type = T0;
    const NAME: &'static str = "0";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0, T1, T2), 0);
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
}

unsafe impl<T0, T1, T2> FieldDescriptor<1> for (T0, T1, T2) {
    type Owner = (T0, T1, T2);
    type Type = T1;
    const NAME: &'static str = "1";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0, T1, T2), 1);
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
}

unsafe impl<T0, T1, T2> FieldDescriptor<2> for (T0, T1, T2) {
    type Owner = (T0, T1, T2);
    type Type = T2;
    const NAME: &'static str = "2";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0, T1, T2), 2);
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
}

unsafe impl<T0, T1, T2> TupleDescriptor for (T0, T1, T2) {
    type Type = (T0, T1, T2);
}

unsafe impl<T0, T1, T2> FieldsAdt for (T0, T1, T2) {
    type Fields = (T0, T1, T2);
    const FIELD_COUNT: usize = 3;
    const FIELD_SYNTAX: FieldSyntax = FieldSyntax::Parentheses;
}

// 4-tuple
unsafe impl<T0, T1, T2, T3> AdtDescriptor for (T0, T1, T2, T3) {
    const ID: AdtId = AdtId::Tuple;
    const NAME: &'static str = type_name::<(T0, T1, T2, T3)>();
}

unsafe impl<T0, T1, T2, T3> FieldDescriptor<0> for (T0, T1, T2, T3) {
    type Owner = (T0, T1, T2, T3);
    type Type = T0;
    const NAME: &'static str = "0";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0, T1, T2, T3), 0);
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
}

unsafe impl<T0, T1, T2, T3> FieldDescriptor<1> for (T0, T1, T2, T3) {
    type Owner = (T0, T1, T2, T3);
    type Type = T1;
    const NAME: &'static str = "1";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0, T1, T2, T3), 1);
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
}

unsafe impl<T0, T1, T2, T3> FieldDescriptor<2> for (T0, T1, T2, T3) {
    type Owner = (T0, T1, T2, T3);
    type Type = T2;
    const NAME: &'static str = "2";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0, T1, T2, T3), 2);
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
}

unsafe impl<T0, T1, T2, T3> FieldDescriptor<3> for (T0, T1, T2, T3) {
    type Owner = (T0, T1, T2, T3);
    type Type = T3;
    const NAME: &'static str = "3";
    // TODO(thephd): monitor bootstrap situation related to std::mem::offset_of
    #[cfg(not(bootstrap))]
    const BYTE_OFFSET: usize = offset_of!((T0, T1, T2, T3), 3);
    #[cfg(bootstrap)]
    const BYTE_OFFSET: usize = 0;
}

unsafe impl<T0, T1, T2, T3> TupleDescriptor for (T0, T1, T2, T3) {
    type Type = (T0, T1, T2, T3);
}

unsafe impl<T0, T1, T2, T3> FieldsAdt for (T0, T1, T2, T3) {
    type Fields = (T0, T1, T2, T3);
    const FIELD_COUNT: usize = 4;
    const FIELD_SYNTAX: FieldSyntax = FieldSyntax::Parentheses;
}
