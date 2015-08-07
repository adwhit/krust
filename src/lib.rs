#![feature(lang_items, slice_bytes, box_patterns)]
#![allow(raw_pointer_derive)]

extern crate libc;
extern crate num;
extern crate nix;

#[macro_use]
extern crate bitflags;

pub mod kbindings;

#[cfg(feature = "api")]
pub mod api;
