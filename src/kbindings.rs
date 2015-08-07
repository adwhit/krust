use libc;
use std;
use std::mem::zeroed;
use std::slice;
use std::ptr;
use std::fmt;
use std::ffi;

pub type S = *const libc::c_char;
pub type C = libc::c_char;
pub type G = libc::c_uchar;
pub type H = libc::c_short;
pub type I = libc::c_int;
pub type J = libc::c_longlong;
pub type E = libc::c_float;
pub type F = libc::c_double;
pub type V = libc::c_void;

#[repr(C)]
pub struct K {
    pub m: libc::c_char,
    pub a: libc::c_char,
    pub t: libc::c_char,
    pub u: C,
    pub r: I,
    pub union: [u8; 16],
}

impl fmt::Debug for K {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut vs = Vec::new();
        vs.push(format!("Type:{}, Attr:{}, RefCt:{} Addr:{:p}",
               self.t, self.u, self.r, self));
        let mut s = String::new();
        for v in self.union.iter() {
            s.push_str(&format!("{:02x}", v))
        }
        vs.push(format!("Union: 0x{}", s));
        f.write_str(&vs.join("\n"))
    }
}

#[derive(Debug)]
pub struct KOwned(pub &'static K);

impl Drop for KOwned {
    fn drop(&mut self) {
        unsafe { r0(self.0) };
    }
}

impl KOwned {
    pub fn new(bytes: &[u8]) -> KOwned {
        unsafe {
            // make a new, empty K struct holding a bytelist
            let k = ktn(4, bytes.len() as i64);
            let sx = (*k).fetch_slice::<u8>();
            assert_eq!(bytes.len(), sx.len());
            slice::bytes::copy_memory(bytes, sx);
            KOwned(&*k)
        }
    }
}


// these are accessors for the (untagged) union
impl K {
    #[inline]
    pub unsafe fn cast<'a, T: fmt::Debug>(&self) -> &'a mut T {
        let u = &self.union as *const u8;
        &mut *(u as *mut T)
    }
    
    pub unsafe fn cast_with_ptr_offset<'a, T>(&self) -> &'a mut T {
        let u = &self.union as *const u8;
        &mut *((u as *const usize).offset(1) as *mut T)
    }

    #[inline]
    pub unsafe fn fetch_slice<'a, T:'a>(&self) -> &'a mut [T] {
        slice::from_raw_parts_mut(self.cast_with_ptr_offset(), *self.cast())
    }
}

impl ::std::default::Default for K {
    fn default() -> Self { unsafe { zeroed() } }
}

#[repr(C)]
pub struct U {
    pub g: [G; 16usize],
}

impl ::std::default::Default for U {
    fn default() -> Self { unsafe { zeroed() } }
}

#[derive(Debug)]
pub enum KData<'a, T: 'a> {
    Atom(&'a mut T),
    List(&'a mut [T])
}

impl<'a, T: 'a + fmt::Debug> KData<'a, T>{
    #[inline]
    unsafe fn atom(k: &'a K) -> KData<'a, T> {
        KData::Atom(k.cast())
    }
    #[inline]
    unsafe fn list(k: &'a K) -> KData<'a, T> {
        KData::List(k.fetch_slice())
    }
}


#[derive(Debug)]
pub enum KVal<'a> {
    Mixed(Vec<KVal<'a>>),
    Bool(KData<'a, bool>),
    Guid(KData<'a, [u8; 16]>),
    Byte(KData<'a, u8>),
    Short(KData<'a, i16>),
    Int(KData<'a, i32>),
    Long(KData<'a, i64>),
    Real(KData<'a, f32>),
    Float(KData<'a, f64>),
    Char(&'a i8),
    String(&'a str),
    Symbol(KData<'a, *const i8>),
    Table(Box<KVal<'a>>),
    Dict(Box<KVal<'a>>, Box<KVal<'a>>), // Keys, Values
    Timestamp(KData<'a, i64>),
    Month(KData<'a, i32>),
    Date(KData<'a, i32>),
    Datetime(KData<'a, f64>),
    Timespan(KData<'a, i64>),
    Minute(KData<'a, i32>),
    Second(KData<'a, i32>),
    Time(KData<'a, i32>),
    Function,
    Unknown,
}

impl<'a> KVal<'a> {
    pub fn new(k: *const K) -> KVal<'a> {
        unsafe {
            let k = &*k;
            match k.t {
                -1 => KVal::Bool(KData::atom(k)),
                -2 => KVal::Guid(KData::atom(k)),
                -4 => KVal::Byte(KData::atom(k)),
                -5 => KVal::Short(KData::atom(k)),
                -6 => KVal::Int(KData::atom(k)),
                -7 => KVal::Long(KData::atom(k)),
                -8 => KVal::Real(KData::atom(k)),
                -9 => KVal::Float(KData::atom(k)),
                -10 => KVal::Char(k.cast()),
                -11 => KVal::Symbol(KData::atom(k)),
                -12 => KVal::Timestamp( KData::atom(k)),
                -13 => KVal::Month( KData::atom(k)),
                -14 => KVal::Date( KData::atom(k)),
                -15 => KVal::Datetime( KData::atom(k)),
                -16 => KVal::Timespan( KData::atom(k)),
                -17 => KVal::Minute( KData::atom(k)),
                -18 => KVal::Second( KData::atom(k)),
                -19 => KVal::Time( KData::atom(k)),
                0 => {
                    let s: &[&K] = k.fetch_slice();
                    KVal::Mixed(s.iter().map(|&x| KVal::new(x)).collect())
                },
                1  => KVal::Bool(   KData::list(k)),
                2  => KVal::Guid(   KData::list(k)),
                4  => KVal::Byte(   KData::list(k)),
                5  => KVal::Short(  KData::list(k)),
                6  => KVal::Int(    KData::list(k)),
                7  => KVal::Long(   KData::list(k)),
                8  => KVal::Real(   KData::list(k)),
                9  => KVal::Float(  KData::list(k)),
                10 => {
                    let s = std::str::from_utf8(k.fetch_slice::<u8>());
                    KVal::String(s.unwrap())
                },
                11 => KVal::Symbol( KData::list(k)),
                12 => KVal::Timestamp( KData::list(k)),
                13 => KVal::Month( KData::list(k)),
                14 => KVal::Date( KData::list(k)),
                15 => KVal::Datetime( KData::list(k)),
                16 => KVal::Timespan( KData::list(k)),
                17 => KVal::Minute( KData::list(k)),
                18 => KVal::Second( KData::list(k)),
                19 => KVal::Time( KData::list(k)),
                98 => KVal::Table(  Box::new(KVal::new(*k.cast::<*const K>()))),
                99 => {
                    let slice = k.fetch_slice::<&K>();
                    KVal::Dict(   Box::new(KVal::new(slice[0])),
                                  Box::new(KVal::new(slice[1])))
                }
                100 => KVal::Function,
                _ => KVal::Unknown
            }
        }
    }

    pub fn to_k(&self) -> &K  {
        match *self {
            KVal::Mixed(ref arr) => kmixed(arr),
            KVal::Bool(KData::Atom(&mut v)) => kbool(v),
            KVal::Bool(KData::List(ref vals)) => klist::<bool>(1, vals),
            KVal::Byte(KData::Atom(&mut v)) => kbyte(v),
            KVal::Byte(KData::List(ref vals)) => klist::<u8>(4, vals),
            KVal::Short(KData::Atom(&mut v)) => kshort(v),
            KVal::Short(KData::List(ref vals)) => klist::<i16>(5, vals),
            KVal::Int(KData::Atom(&mut v)) => kint(v),
            KVal::Int(KData::List(ref vals)) => klist::<i32>(6, vals),
            KVal::Long(KData::Atom(&mut v)) => klong(v),
            KVal::Long(KData::List(ref vals)) => klist::<i64>(7, vals),
            KVal::Real(KData::Atom(&mut v)) => kreal(v),
            KVal::Real(KData::List(ref vals)) => klist::<f32>(8, vals),
            KVal::Float(KData::Atom(&mut v)) => kfloat(v),
            KVal::Float(KData::List(ref vals)) => klist::<f64>(9, vals),
            //KVal::Symbol(KData::Atom(&mut v)) => ksymbol(v),
            KVal::Symbol(KData::List(ref vals)) => klist::<*const i8>(11, vals),
            KVal::Dict(box ref k, box ref v) => kdict(k, v),
            _ => kerror("NYI")
        }
    }

}

pub fn intern_strings(strs: Vec<String>) -> Vec<*const i8> {
    unsafe {
        strs.into_iter()
            .map(|s| ss(ffi::CString::new(s).unwrap().as_ptr()))
            .collect() 
    }
}

pub fn valid_stream(k: &K) -> bool {
    unsafe { okx(k) == 1 }
}

pub fn deserial(k: &K) -> &K  {
    unsafe { &*d9(k) }
}

pub fn kerror(err: &str) -> &'static K {
    // AFAICT, just returns a null pointer
    unsafe { &*krr(ffi::CString::new(err).unwrap().as_ptr()) }
}

pub fn kbool(b: bool) -> &'static K {
    unsafe { &*kb( { if b { 1 } else { 0 } } ) }
}

pub fn kbyte(b: u8) -> &'static K {
    unsafe { &*kg(b as i32) }
}

pub fn kshort(h: i16) -> &'static K {
    unsafe { &*kh(h as i32) }
}

pub fn kint(i: i32) -> &'static K {
    unsafe { &*ki(i) }
}

pub fn klong(j: i64) -> &'static K {
    unsafe { &*kj(j) }
}

pub fn kreal(e: f32) -> &'static K {
    unsafe { &*ke(e as f64) }
}

pub fn kfloat(f: f64) -> &'static K {
    unsafe { &*kf(f) }
}

pub fn kchar(c: char) -> &'static K {
    unsafe { &*kc(c as i32) }
}

pub fn kstring(s: &str) -> &'static K {
     unsafe { &*kpn(ffi::CString::new(s).unwrap().as_ptr(), s.len() as i64) }
}

pub fn ksymbol(s: &str) -> &'static K {
    unsafe { &*(ks(ffi::CString::new(s).unwrap().as_ptr())) }
}

pub fn kvoid() -> *const K {
    ptr::null()
}

fn klist<T>(ktype: i32, vals: &[T]) -> &'static K {
    unsafe {
        let k = ktn(ktype, vals.len() as i64);
        let mut sx = (*k).fetch_slice::<T>();
        assert_eq!(vals.len(), sx.len());
        std::ptr::copy_nonoverlapping(vals.as_ptr(), sx.as_mut_ptr(), vals.len());
        &*k
    }
}

pub fn kdict(keys: &KVal, vals: &KVal) -> &'static K {
    unsafe { &*xD(keys.to_k(), vals.to_k()) }
}

pub fn ktable(dict: KVal) -> &'static K {
    unsafe { &*xT(dict.to_k()) }
}

fn kmixed(vals: &[KVal]) -> &'static K {
    let (k, sx);
    unsafe {
        k = &*ktn(0, vals.len() as i64);
        sx = (*k).fetch_slice::<&K>();
    }
    assert_eq!(vals.len(), sx.len());
    for (ix, val) in vals.iter().enumerate() {
        sx[ix] = val.to_k()
    }
    k
}

/* TODO
    pub fn ks(arg1: S) -> *const K;                // create symbol
    pub fn kd(arg1: I) -> *const K;                // create date
    pub fn kz(arg1: F) -> *const K;                // create datetime
    pub fn kt(arg1: I) -> *const K;                // create time
    pub fn ku(arg1: U) -> *const K;                // create guid
    pub fn ka(arg1: I) -> *const K;                // create atom
    pub fn ktn(arg1: I, arg2: J) -> *const K;      // create list
    pub fn knk(arg1: I, ...) -> *const K;          // create mixed list
    pub fn ktj(arg1: I, arg2: J) -> *const K;      // create timestamp
    pub fn kp(arg1: S) -> *const K;                // create string
    pub fn kpn(arg1: S, arg2: J) -> *const K;      // create string length n
    pub fn xT(arg1: K) -> *const K;                // create table from dict
    pub fn xD(arg1: K, arg2: K) -> *const K;       // create dict
    pub fn ktd(arg1: K) -> *const K;               // simple table from keyed table
*/

// the difference between the configurations is that api a) links to libkdb.a and b) includes ipc
#[cfg(feature="api")]
#[link(name="kdb")]
extern "C" {
    pub fn ktn(arg1: I, arg2: J) -> *const K;      // create list
    pub fn knk(arg1: I, ...) -> *const K;          // create mixed list
    pub fn ku(arg1: U) -> *const K;                // create guid
    pub fn ka(arg1: I) -> *const K;                // create atom
    pub fn kb(arg1: I) -> *const K;                // create boolean
    pub fn kg(arg1: I) -> *const K;                // create byte
    pub fn kh(arg1: I) -> *const K;                // create short
    pub fn ki(arg1: I) -> *const K;                // create int
    pub fn kj(arg1: J) -> *const K;                // create long
    pub fn ke(arg1: F) -> *const K;                // create real
    pub fn kf(arg1: F) -> *const K;                // create float
    pub fn kc(arg1: I) -> *const K;                // create char
    pub fn ks(arg1: S) -> *const K;                // create symbol
    pub fn kd(arg1: I) -> *const K;                // create date
    pub fn kz(arg1: F) -> *const K;                // create datetime
    pub fn kt(arg1: I) -> *const K;                // create time
    pub fn ktj(arg1: I, arg2: J) -> *const K;      // create timestamp
    pub fn kp(arg1: S) -> *const K;                // create string
    pub fn kpn(arg1: S, arg2: J) -> *const K;      // create string length n
    pub fn xT(arg1: *const K) -> *const K;                // create table from dict
    pub fn xD(arg1: *const K, arg2: *const K) -> *const K;       // create dict
    pub fn ktd(arg1: *const K) -> *const K;               // simple table from keyed table

    pub fn ss(arg1: S) -> S;                // intern a string
    pub fn sn(arg1: S, arg2: I) -> S;       // intern n chars from string

    pub fn ymd(arg1: I, arg2: I, arg3: I) -> I;     // encode year/month/day as int
    pub fn dj(arg1: I) -> I;                        // create date from int

    pub fn setm(arg1: I) -> I;

    // IPC
    pub fn khp(arg1: S, arg2: I) -> I;                      // connect to server
    pub fn khpu(arg1: S, arg2: I, arg3: S) -> I;            // connect with username
    pub fn khpun(arg1: S, arg2: I, arg3: S, arg4: I) -> I;  // connect with username, timeout
    pub fn kclose(arg1: I) -> V;            // close socket
    pub fn k(arg1: I, arg2: S, ...) -> *const K;   // remote execution

    pub fn r1(arg1: K) -> *const K;                // increment ref count
    pub fn r0(arg1: *const K) -> V;                // decrement ref count
    pub fn m9() -> V;                       // garbage collect (?)
    pub fn sd1(arg1: I, arg2: Option<extern "C" fn(arg1: I) -> *const K>) -> *const K; // set callback
    pub fn sd0(arg1: I) -> V;                                            // remove callback

    pub fn dl(f: *mut V, arg1: I) -> *const K;     // dynamic link

    pub fn ja(arg1: *mut K, arg2: *mut V) -> *const K;     // join atom to list
    pub fn js(arg1: *mut K, arg2: S) -> *const K;          // join string to list
    pub fn jk(arg1: *mut K, arg2: K) -> *const K;          // join k obj to list
    pub fn jv(k: *mut K, arg1: K) -> *const K;             // join two lists

    pub fn krr(arg1: S) -> *const K;                       // raise error
    pub fn orr(arg1: S) -> *const K;                       // raise system error
    pub fn dot(arg1: K, arg2: K) -> *const K;      // 'dot' function (apply-over)

    pub fn okx(arg1: *const K) -> I;               // check byte stream valid
    pub fn b9(arg1: I, arg2: *const K) -> *const K;       // serialize object
    pub fn d9(arg1: *const K) -> *const K;                // deserialize byte stream
}

#[cfg(not(feature="api"))]
extern "C" {
    pub fn ktn(arg1: I, arg2: J) -> *const K;      // create list
    pub fn knk(arg1: I, ...) -> *const K;          // create mixed list
    pub fn ku(arg1: U) -> *const K;                // create guid
    pub fn ka(arg1: I) -> *const K;                // create atom
    pub fn kb(arg1: I) -> *const K;                // create boolean
    pub fn kg(arg1: I) -> *const K;                // create byte
    pub fn kh(arg1: I) -> *const K;                // create short
    pub fn ki(arg1: I) -> *const K;                // create int
    pub fn kj(arg1: J) -> *const K;                // create long
    pub fn ke(arg1: F) -> *const K;                // create real
    pub fn kf(arg1: F) -> *const K;                // create float
    pub fn kc(arg1: I) -> *const K;                // create char
    pub fn ks(arg1: S) -> *const K;                // create symbol
    pub fn kd(arg1: I) -> *const K;                // create date
    pub fn kz(arg1: F) -> *const K;                // create datetime
    pub fn kt(arg1: I) -> *const K;                // create time
    pub fn ktj(arg1: I, arg2: J) -> *const K;      // create timestamp
    pub fn kp(arg1: S) -> *const K;                // create string
    pub fn kpn(arg1: S, arg2: J) -> *const K;      // create string length n
    pub fn xT(arg1: *const K) -> *const K;                // create table from dict
    pub fn xD(arg1: *const K, arg2: *const K) -> *const K;       // create dict
    pub fn ktd(arg1: *const K) -> *const K;               // simple table from keyed table

    pub fn ss(arg1: S) -> S;                // intern a string
    pub fn sn(arg1: S, arg2: I) -> S;       // intern n chars from string

    pub fn ymd(arg1: I, arg2: I, arg3: I) -> I;     // encode year/month/day as int
    pub fn dj(arg1: I) -> I;                        // create date from int

    pub fn setm(arg1: I) -> I;

    pub fn r1(arg1: K) -> *const K;                // increment ref count
    pub fn r0(arg1: *const K) -> V;                // decrement ref count
    pub fn m9() -> V;                       // garbage collect (?)
    pub fn sd1(arg1: I, arg2: Option<extern "C" fn(arg1: I) -> *const K>) -> *const K; // set callback
    pub fn sd0(arg1: I) -> V;                                            // remove callback

    pub fn dl(f: *mut V, arg1: I) -> *const K;     // dynamic link

    pub fn ja(arg1: *mut K, arg2: *mut V) -> *const K;     // join atom to list
    pub fn js(arg1: *mut K, arg2: S) -> *const K;          // join string to list
    pub fn jk(arg1: *mut K, arg2: K) -> *const K;          // join k obj to list
    pub fn jv(k: *mut K, arg1: K) -> *const K;             // join two lists

    pub fn krr(arg1: S) -> *const K;                       // raise error
    pub fn orr(arg1: S) -> *const K;                       // raise system error
    pub fn dot(arg1: K, arg2: K) -> *const K;      // 'dot' function (apply-over)

    pub fn okx(arg1: *const K) -> I;               // check byte stream valid
    pub fn b9(arg1: I, arg2: *const K) -> *const K;       // serialize object
    pub fn d9(arg1: *const K) -> *const K;                // deserialize byte stream
}
