#![no_std]

use ::core::marker::PhantomData;

pub mod reexports {
    //! Helpful const crates to use in your code.

    pub use ::const_format;
    // pub use ::konst;
}

/// A trait that is implemented by all enum arrays, for easier use in your traits and generics.
/// ```
/// use enum_array::EnumArray;
/// use core::str::FromStr;
///
/// pub struct ActionMap<T: EnumArray> {
///     on_click: Option<T>,
///     on_right_click: Option<T>,
/// }
///
/// impl<T: EnumArray> ActionMap<T> {
///     pub fn set_click_action(&mut self, configured_action: &str) -> &mut Self {
///         self.on_click = T::from_str(configured_action).ok();
///         self
///     }
/// }
/// ```
pub trait EnumArray:
    Sized + 'static + ::core::fmt::Display + Copy + Eq + AsRef<str> + ::core::str::FromStr
{
    /// Get a reference to the array of all variants in this enum. Used for iterators.
    ///
    /// TODO: Make this an array referencing [`EnumArray::NUM_VARIANTS`]
    fn slice() -> &'static [Self];

    /// An inner implementation detail of `try_from_str`.
    ///
    /// TODO: Make this trait function const
    fn try_from_str(s: &str) -> Option<Self>;

    /// The number of variants in this enum.
    const NUM_VARIANTS: usize;

    /// Iterate over this enum's variants
    fn iter() -> EnumIter<Self> {
        EnumIter {
            idx: 0,
            _marker: PhantomData,
        }
    }
}

/// Create an enum with various helper functions and methods.
///
/// Since neither const generics nor const traits are stable yet, various functions are directly implemented on the enum instead.
///
/// ```
/// use enum_array::enum_array;
///
/// enum_array! {
///     enum SimpleEnum {
///         Red,
///         Green,
///         Blue,
///     }
/// }
///
/// // it automatically implements Display
/// assert_eq!(SimpleEnum::Red.to_string(), SimpleEnum::Red.as_static_str());
///
/// ```
///
/// Each enum supports various attributes:
///
/// ```
///
/// enum_array::enum_array! {
///     // The inner implementation already calls derive, but you can add more if you like.
///     #[derive(Default)]
///     #[doc = "A more involved example enum"]
///     enum HelloWorld {
///         #[default]
///         Hello,
///         World = 7,
///         [@str = "whaaaaaaat"]
///         What = 98,
///         Is = 13,
///         Up,
///     }
/// }
///
/// assert_eq!(HelloWorld::What as usize, 98);
/// assert_eq!(HelloWorld::Up.as_static_str(), "Up");
/// assert_eq!(HelloWorld::What.as_static_str(), "whaaaaaaat");
///
/// ```
///
/// You can even map each enum variant to a different case! (using the [`const_format`] crate internally)
///
/// ```
/// // use the trait EnumArray to get access to the `iter` method
/// use enum_array::{enum_array, EnumArray};
/// use core::str::FromStr;
///
/// enum_array! {
///     #[doc = "An example enum, using case conversion"]
///     #[derive(Default)]
///     #[repr(u8)]
///     pub enum HelloWorldKebabCase
///     @case: Kebab {
///         #[default]
///         HelloWorld,
///         WorldWhat = 7,
///         [@str = "whaaaaaaatIS"]
///         WhatIs,
///         IsUp = 99,
///     }
/// }
///
/// assert_eq!(HelloWorldKebabCase::WorldWhat.as_static_str(), "world-what");
///
/// assert!(HelloWorldKebabCase::from_str("HelloWorld").is_err());
/// assert_eq!(HelloWorldKebabCase::from_str("whaaaaaaatIS"), Ok(HelloWorldKebabCase::WhatIs));
///
/// for variant in HelloWorldKebabCase::iter() {
///     println!("{}", variant);
/// }
///
/// ```
#[macro_export]
macro_rules! enum_array {
    (
        $( #[$enum_meta:meta] )*
        $vis:vis enum $enum_name:ident
        $( @case: $case:ident )?
        { $(
            $( #[$variant_meta:meta] )*
            $([@str = $variant_str:expr])?
            $variant_name:ident $(= $variant_value:expr)?
        ),*$(,)? } ) => {
        $(#[$enum_meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        $vis enum $enum_name {
            $( $( #[$variant_meta] )* $variant_name $(= $variant_value)? ),*
        }

        impl $enum_name {
            /// Returns an array of all variants in this enum.
            pub const VARIANT_ARRAY: [Self; <Self as $crate::EnumArray>::NUM_VARIANTS] = [$( Self::$variant_name ),+];

            /// An array of the names (strings) of all variants in this enum.
            pub const VARIANT_NAMES: [&'static str; <Self as $crate::EnumArray>::NUM_VARIANTS] =
                $crate::enum_array![@inner_map_case_array $($case)? [$( $variant_name $(= $variant_str )? ),+]];

            pub const fn as_static_str(&self) -> &'static str {
                $crate::enum_array![@inner_map_case_match (self) $($case)? [$( $variant_name $(= $variant_str )? ),+]]
            }
        }

        impl ::core::fmt::Display for $enum_name {
            #[inline(always)]
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                f.write_str(self.as_static_str())
            }
        }

        impl ::core::convert::AsRef<str> for $enum_name {
            #[inline(always)]
            fn as_ref(&self) -> &str {
                self.as_static_str()
            }
        }

        impl ::core::str::FromStr for $enum_name {
            type Err = ();
            #[inline(always)]
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                <Self as $crate::EnumArray>::try_from_str(s).ok_or(())
            }
        }

        impl $crate::EnumArray for $enum_name {
            const NUM_VARIANTS: usize = *&[$( Self::$variant_name ),+].len();
            #[inline(always)]
            fn slice() -> &'static [Self] {
                &Self::VARIANT_ARRAY
            }

            #[allow(non_upper_case_globals)]
            fn try_from_str(s: &str) -> Option<Self> {
                $crate::enum_array![@inner_map_case_fromstr $($case)? [$( $variant_name $(= $variant_str )? ),+]];

                match s {
                    $(
                        $variant_name => Some(Self::$variant_name),
                    )*
                    _ => None
                }
            }
        }
    };

    (@inner_map_case $variant:ident) => {
        ::core::stringify!($variant)
    };
    (@inner_map_case ($case:ident) $variant:ident) => {
        $crate::reexports::const_format::map_ascii_case!($crate::reexports::const_format::Case::$case, ::core::stringify!($variant))
    };

    (@inner_map_case $( ($case:ident) )? $variant:ident = $override:expr) => {
        $override
    };

    // workaround for https://github.com/rust-lang/rust/issues/96184
    (@inner_map_case_array [$( $variant:ident $(= $override:expr)? ),+]) => {
        [ $( $crate::enum_array![@inner_map_case $variant $( = $override )?] ),+ ]
    };
    (@inner_map_case_array $case:ident [$( $variant:ident $(= $override:expr)? ),+]) => {
        [ $( $crate::enum_array![@inner_map_case ($case) $variant $( = $override )?] ),+ ]
    };

    (@inner_map_case_match ($enum_value:expr) [$( $variant:ident $(= $override:expr)? ),+]) => {
        match $enum_value {
            $(
                Self::$variant =>
                    $crate::enum_array![@inner_map_case $variant $( = $override )?],
            )+
        }
    };
    (@inner_map_case_match ($enum_value:expr) $case:ident [$( $variant:ident $(= $override:expr)? ),+]) => {
        match $enum_value {
            $(
                Self::$variant =>
                    $crate::enum_array![@inner_map_case ($case) $variant $( = $override )?],
            )+
        }
    };

    (@inner_map_case_fromstr [$( $variant:ident $(= $override:expr)? ),+]) => {
        $(
            const $variant: &str = $crate::enum_array![@inner_map_case $variant $( = $override )?];
        )+
    };
    (@inner_map_case_fromstr $case:ident [$( $variant:ident $(= $override:expr)? ),+]) => {
        $(
            const $variant: &str = $crate::enum_array![@inner_map_case ($case) $variant $( = $override )?];
        )+
    };
}

/// An iterator over the variants of an enum.
/// ```
/// use enum_array::{enum_array, EnumIter, EnumArray};
///
/// enum_array! {
///     enum SimpleEnum {
///         Red,
///         Green,
///         Blue,
///     }
/// }
///
/// for variant in SimpleEnum::iter() {
///     println!("{}", variant);
/// }
/// ```
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumIter<T: EnumArray> {
    _marker: PhantomData<T>,
    idx: usize,
}
impl<T: EnumArray> Iterator for EnumIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < T::NUM_VARIANTS {
            let out = T::slice()[self.idx];
            self.idx += 1;

            Some(out)
        } else {
            None
        }
    }
}
