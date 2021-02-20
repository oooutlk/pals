// Copyright 2018 oooutlk@outlook.com. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! # pals = Processes' Arguments LiSt
//!
//! The main API is `pals()` which returns `ProcList` if succeeded.
//!
//! ```rust,no_run
//! let proc_list = pals::pals().unwrap();
//! ```
//!
//! ## `ProcList` can be regarded as a list of `procs()`.
//!
//! ```rust,no_run
//! let proc_list = pals::pals().unwrap();
//! let procs = proc_list.procs();
//! ```
//!
//! The following items can be accessed:
//!
//! 1. pid -- Process::pid.
//!
//! 2. ppid -- Process::ppid.
//!
//! 3. nul-terminated arguments in one single string -- Process::arguments.
//!
//! 4. argument iterator -- `Process::argv()`.
//!
//! 5. parent process -- `ProcList::parent_of()`.
//!
//! ```rust,no_run
//! let proc_list = pals::pals().unwrap();
//! let mut procs = proc_list.procs();
//! let first = procs.next().unwrap();
//! println!( "pid:{:?}, ppid:{:?}", first.pid, first.ppid );
//! println!( "arguments:{}", first.arguments );
//! println!( "argv:\n{}", first
//!     .argv()
//!     .enumerate()
//!     .fold( String::new(),
//!         |acc,(i,arg)| format!( "{}\narg #{}:{}", acc, i, arg ))
//! );
//! println!( "parent's pid:{:?}", proc_list.parent_of( first.pid ));
//! ```
//!
//! ## `ProcList` can be regarded as a list of `Proc` trees.
//!
//! Besides items mentioned above, the following extra items can be accessed:
//!
//! 6. parent nodes -- `Proc::parent()`.
//!
//! 7. all child nodes -- `Proc::children()`.
//!
//! ```rust
//! use pals::{Proc, pals};
//!
//! pals().map( |proc_list| {
//!     fn assert_ppid_is_parent_pid( proc: Proc ) {
//!         proc.parent()
//!             .map( |parent| assert_eq!( parent.pid, proc.ppid ));
//!         proc.children()
//!             .for_each( |subproc| assert_ppid_is_parent_pid( subproc ));
//!     }
//!
//!     proc_list
//!         .children()
//!         .for_each( |proc| assert_ppid_is_parent_pid( proc ))
//! }).unwrap();
//! ```
//!
//! ## `ProcList` can be converted to `trees::Forest`.
//!
//! ```rust,no_run
//! use pals::{Process, pals};
//! use trees::Forest;
//!
//! let proc_list = pals().unwrap();
//!
//! let bfs = proc_list
//!     .bfs()
//!     .map( ToOwned::to_owned ); // &Process -> Process
//!
//! let forest = Forest::<Process>::from( bfs );
//! ```
//!
//! # Binary utility
//!
//! See README.md for more.

use anyhow::anyhow;

#[cfg( unix )]
use anyhow::Context;

#[cfg( target_os = "freebsd" )]
use freebsd::parse_id;

#[cfg( target_os = "freebsd" )]
use serde::{Deserialize, de};

use std::{
    fmt::{self, Debug, Display},
    mem,
    ops::Deref,
    str,
};

#[cfg( not( target_os = "windows" ))]
use std::process::Command;

use trees::bfs::{BfsForest, Split, Splitted};

/// Process ID.
#[derive( Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord )]
#[cfg_attr( target_os = "freebsd", derive( Deserialize ))]
pub struct Pid( pub u32 );

#[cfg( target_os = "freebsd" )]
mod freebsd;
#[cfg( target_os = "freebsd" )]
use freebsd::{PsOutput, pals_from_procfs};

#[cfg( target_os = "linux" )]
mod linux;
#[cfg( target_os = "linux" )]
use linux::pals_from_procfs;

#[cfg( windows )]
mod windows;

/// Process list. It can be viewed as a list of process or a list of process
/// trees.
#[derive( Debug )]
#[cfg_attr( target_os = "freebsd", derive( Deserialize ))]
pub struct ProcList {
    #[cfg_attr( target_os = "freebsd", serde( rename = "process" ))]
    procs : Vec<ProcNode>,
    #[cfg_attr( target_os = "freebsd", serde( skip ))]
    root  : Link,
}

impl Default for ProcList {
    fn default() -> Self { ProcList{ procs: Vec::new(), root: Link::default() }}
}

impl ProcList {
    /// Gets parent process' pid of the process the pid of which is given.
    pub fn parent_of( &self, pid: Pid ) -> Option<&Process> {
        self.locate( pid ).and_then( |index| {
            let process = &self.procs[ index ].process;
            if process.pid != process.ppid {
                Some( process )
            } else {
                None
            }
        })
    }

    fn locate( &self, pid: Pid ) -> Option<usize> {
        self.procs.binary_search_by_key( &pid, ProcNode::id ).ok()
    }

    fn adopt( &mut self, parent: Option<usize>, child: usize ) -> anyhow::Result<()> {
        let mut index = parent;
        let child_pid = self.procs[ child ].pid;
        let node_count = self.procs[ child ].link.size.descendants + 1;
        while let Some( i ) = index {
            self.procs[i].link.size.descendants += node_count;
            let pid = self.procs[i].pid;
            if pid == self.procs[i].ppid {
                break;
            } else if pid == child_pid {
                return Err( anyhow!( "error: {:?} caused circuit in process tree.", pid ));
            }
            index = self.locate( self.procs[i].ppid );
        }

        match parent {
            Some( parent ) => {
                self.procs[ parent ].link.size.degree += 1;
                self.procs[ child ].link.sib = self.procs[ parent ].link.child;
                self.procs[ child ].link.parent = parent;
                self.procs[ parent ].link.child = child;
            },
            None => {
                self.root.size.degree += 1;
                self.procs[ child ].link.sib = self.root.child;
                self.root.child = child;
            },
        }

        self.root.size.descendants += node_count;

        Ok(())
    }

    /// Returns all detected running processes.
    ///
    /// # Examples
    ///
    /// ```text
    /// .............
    /// .  ProcList .
    /// .   /   \   .
    /// .  1     4  .
    /// . / \   / \ .
    /// .2   3 5   6.
    /// .............
    /// ```
    ///
    /// This method will returns #1, #2, #3, #4, #5, #6.
    pub fn procs( &self ) -> impl Iterator<Item=&ProcNode> {
        self.procs.iter()
    }

    /// Returns all detected running process trees.
    ///
    /// # Examples
    ///
    /// ```text
    /// .............
    /// .  ProcList .
    /// .   /   \   .
    /// .  1     4  .
    /// . / \   / \ .
    /// .2   3 5   6.
    /// .............
    /// ```
    ///
    /// This method will returns #1 and #4.
    ///
    /// Use `.children()` recursively to get descendant processes, e.g. #2.
    pub fn children<'a, 's:'a>( &'s self ) -> Proc<'a> {
        Proc {
            proc_list : self,
            index     : self.root.child,
            degree    : self.root.size.degree,
        }
    }

    /// Returns processes in breadth first search. This method helps to convert
    /// `ProcList` into `trees::Forest`.
    ///
    ///  See [trees](https://crates.io/crates/trees) for more.
    pub fn bfs<'a, 's:'a>( &'s self ) -> BfsForest<Splitted<Proc<'a>>> {
        BfsForest::from( self.children(), self.root.size )
    }
}

impl Display for ProcList {
    fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
        f.write_str( "[" )?;
        let mut children = self.children();
        if let Some( first ) = children.next() {
            first.display( f, 1, true, true )?;
        }
        for child in children {
            child.display( f, 1, false, false )?;
        }
        f.write_str( "]" )
    }
}

/// A process agent by which pid,ppid,arguments/argv, parent and child
/// processes can be accessed.
#[derive( Copy, Clone )]
pub struct Proc<'a> {
    proc_list : &'a ProcList,
    index     : usize,
    degree    : usize,
}

impl<'a> Proc<'a> {
    /// Gets parent process.
    pub fn parent( &self ) -> Option<Self> {
        let parent = self.proc_list.procs[ self.index ].link.parent;
        if parent == Link::null() {
            None
        } else {
            let degree = self.proc_list.procs[ parent ].link.size.degree;
            Some( Proc{ proc_list: self.proc_list, index: parent, degree })
        }
    }

    /// Gets child processes.
    pub fn children( &self ) -> Self {
        let link = &self.proc_list.procs[ self.index ].link;
        Proc {
            proc_list : self.proc_list,
            index     : link.child,
            degree    : link.size.degree,
        }
    }

    fn display( &self, f: &mut fmt::Formatter, ident: usize, is_first_child: bool, is_first_line: bool ) -> fmt::Result {
        if !is_first_line {
            if is_first_child {
                f.write_str("\n")?;
            } else {
                f.write_str(",\n")?;
            }
            for _ in 0..ident {
                f.write_str(" ")?;
            }
        }
        let process = &self.proc_list.procs[ self.index ].process;
        write!( f, "{{cmd: \"{}\", pid:{}"
            , escape8259::escape( &process.command )
            , process.pid.0
        )?;

        let mut ends_with_pid = true;

        let argv = process.argv_to_string();
        if !argv.is_empty() {
            ends_with_pid = false;
            write!( f, ", args:[{}]", argv )?;
        }

        let mut subprocs = self.children();
        if let Some( first ) = subprocs.next() {
            ends_with_pid = false;
            f.write_str( ", subs:[" )?;
            first.display( f, ident+1, true, false )?;
            for proc in subprocs {
                proc.display( f, ident+1, false, false )?;
            }
            f.write_str("]")?;
        }
        if ends_with_pid {
            f.write_str(" ")?;
        }
        f.write_str("}")
    }
}

impl<'a> Iterator for Proc<'a> {
    type Item = Self;

    fn next( &mut self ) -> Option<Self::Item> {
        if self.index == Link::null() {
            None
        } else {
            let index = self.index;
            let link = &self.proc_list.procs[ index ].link;
            self.index = link.sib;
            self.degree -= 1;
            Some( Proc{ proc_list: self.proc_list, index, degree: link.size.degree })
        }
    }

    fn size_hint( &self) -> (usize, Option<usize>) {
        (self.degree, Some(self.degree) )
    }
}

impl<'a> ExactSizeIterator for Proc<'a> {}

impl<'a> Deref for Proc<'a> {
    type Target = Process;

    fn deref( &self ) -> &Process {
        if self.index == Link::null() {
            panic!( "bad deref to `Proc`." );
        }
        &self.proc_list.procs[ self.index ]
    }
}

impl<'a> Debug for Proc<'a> {
    fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
        self.deref().fmt( f )
    }
}

impl<'a> Split for Proc<'a> {
    type Item = &'a Process;
    type Iter = Self;

    fn split( self ) -> (Self::Item, Self::Iter, usize) {
        let process = &self.proc_list.procs[ self.index ];
        let descendants = process.link.size.descendants;
        (process, self.children(), descendants)
    }
}

#[doc( hidden )]
#[derive( Debug )]
#[cfg_attr( target_os = "freebsd", derive( Deserialize ))]
pub struct ProcNode {
    #[cfg_attr( target_os = "freebsd", serde( flatten ))]
    pub process : Process,
    #[cfg_attr( target_os = "freebsd", serde( skip ))]
        link    : Link,
}

impl Deref for ProcNode {
    type Target = Process;

    fn deref( &self ) -> &Process { &self.process }
}

/// Process holding pid, ppid, command name and arguments of one or more
///  nul-terminated argument(s).
#[derive( Clone, Debug, PartialEq, Eq )]
#[cfg_attr( target_os = "freebsd", derive( Deserialize ))]
pub struct Process {
    #[cfg_attr( target_os = "freebsd", serde( deserialize_with = "parse_id" ))]
    pub pid       : Pid,
    #[cfg_attr( target_os = "freebsd", serde( deserialize_with = "parse_id" ))]
    pub ppid      : Pid,
    pub command   : String,
    pub arguments : String,
}

impl ProcNode {
    fn id( &self ) -> Pid { self.pid }

    pub(crate) fn from( process: Process ) -> Self { ProcNode{ process, link: Link::default() }}
}

fn argv_iter( argv: &str ) -> impl Iterator<Item=&str> {
    argv.trim_end_matches('\0').split( '\0' )
}

impl Process {
    /// Returns arguments iterator.
    pub fn argv( &self ) -> impl Iterator<Item=&str> {
        argv_iter( &self.arguments )
    }

    fn argv_to_string( &self ) -> String {
        let mut s = String::new();
        if !self.arguments.is_empty() {
            let mut argv = self.argv();
            if let Some( arg ) = argv.next() {
                s.push_str( &format!( " \"{}\"", escape8259::escape( arg )));
                for arg in argv {
                    s.push_str( &format!( ", \"{}\"", escape8259::escape( arg )));
                }
                s.push(' ');
            }
        }
        s
    }
}

#[derive( Debug )]
struct Link {
    size   : trees::Size,
    parent : usize,
    child  : usize,
    sib    : usize,
}

impl Link {
    const fn null() -> usize { std::usize::MAX }
}

impl Default for Link {
    fn default() -> Self {
        Link {
            parent : Link::null(),
            child  : Link::null(),
            sib    : Link::null(),
            size   : trees::Size::default(),
        }
    }
}

/// Dumps running processes' arguments into a list/forest.
#[cfg( target_os = "freebsd" )]
pub fn pals() -> anyhow::Result<ProcList> {
    pals_from_procfs( "/proc" )
        .or_else( |_| pals_from_ps_json() )
        .or_else( |_| pals_from_ps_ww() )
}

/// Dumps running processes' arguments into a list/forest.
#[cfg( target_os = "linux" )]
pub fn pals() -> anyhow::Result<ProcList> {
    pals_from_procfs( "/proc" )
        .or_else( |_| pals_from_ps_ww() )
}

/// Dumps running processes' arguments into a list/forest.
#[cfg( target_os = "windows" )]
pub fn pals() -> anyhow::Result<ProcList> {
    windows::pals_from_wmic()
}

/// Dumps running processes' arguments into a list/forest.
#[cfg( not( any( target_os = "freebsd",  target_os = "linux",  target_os = "windows" )))]
pub fn pals() -> anyhow::Result<ProcList> {
    pals_from_ps_ww()
}

#[cfg( not( target_os = "windows" ))]
pub(crate) fn pals_from_ps_ww() -> anyhow::Result<ProcList> {
    let output = String::from_utf8(
        Command::new("ps")
            .args( &[ "awwo", "pid,ppid,comm,args" ])
            .output()?
            .stdout
    )?;
    from_table( &output )
}

#[cfg( target_os = "freebsd" )]
fn pals_from_ps_json() -> anyhow::Result<ProcList> {
    let output = String::from_utf8(
        Command::new("ps")
            .args( &[ "a", "--libxo", "json", "-o", "pid,ppid,comm,args" ])
            .output()?
            .stdout
    )?;
    from_json( &output )
}

#[cfg( target_os = "freebsd" )]
fn from_json( json: &str ) -> anyhow::Result<ProcList> {
    let ps_output: PsOutput = serde_json::from_str( json )?;

    build_tree( ps_output.proc_list, ArgvStatus::Parsing )
}

#[cfg( unix )]
fn from_table( table: &str ) -> anyhow::Result<ProcList> {
    let mut lines = table.lines();
    let header = lines.next().context("ps command should generate some output.")?;

    let ppid_col = header.find(" PID")
        .context( "PID column should be generated by ps command.")? + 4;
    let comm_col = header.find(" PPID")
        .context("PPID column should be generated by ps command.")? + 5;
    let args_col = header.rfind("COMMAND")
        .context("COMMAND column should be generated by ps command.")?;

    let procs = lines.try_fold( Vec::new(), |mut procs, line| -> anyhow::Result<Vec<ProcNode>> {
        let bytes     = line.as_bytes();
        let pid       = Pid( u32::from_str_radix( str::from_utf8( &bytes[         ..ppid_col ])?.trim(), 10 )? );
        let ppid      = Pid( u32::from_str_radix( str::from_utf8( &bytes[ ppid_col..comm_col ])?.trim(), 10 )? );
        let command   = str::from_utf8( &bytes[ comm_col..args_col ])?.trim().to_owned();
        let arguments = str::from_utf8( &bytes[ args_col..         ])?.trim().to_owned();

        procs.push( ProcNode::from( Process{ pid, ppid, command, arguments }));
        Ok( procs )
    })?;

    build_tree( ProcList{ procs, root: Link::default() }, ArgvStatus::Parsing )
}

enum ArgvStatus {
    Parsing,
    #[cfg( unix )]
    Splitted,
}

impl ArgvStatus {
    fn needs_parsing( &self ) -> bool {
        match self {
            ArgvStatus::Parsing  => true,
            #[cfg( unix )]
            ArgvStatus::Splitted => false,
        }
    }
}

fn build_tree( mut proc_list: ProcList, argv_status: ArgvStatus ) -> anyhow::Result<ProcList> {
    proc_list.procs.sort_unstable_by_key( ProcNode::id );

    for i in 0..proc_list.procs.len() {
        let parent = if proc_list.procs[i].pid != proc_list.procs[i].ppid {
            proc_list.locate( proc_list.procs[i].ppid )
        } else {
            None
        };
        proc_list.adopt( parent, i )?;

        if argv_status.needs_parsing() {
            let arguments = &mut proc_list.procs[i].process.arguments;

            #[cfg( unix )]
            {
                let mut args = String::with_capacity( arguments.len() + 8 );
                mem::swap( &mut args, arguments );
                let mut splitted_args = cmdline_words_parser::parse_posix( &mut args );
                while let Some( arg ) = splitted_args.next() {
                    arguments.push_str( arg );
                    arguments.push( '\0' );
                }
            }

            #[cfg( windows )]
            {
                let mut splitted_argv = win_argv( arguments );
                mem::swap( arguments, &mut splitted_argv );
            }
        }
    }
    Ok( proc_list )
}

/// Splits command line into arguments, aka argv, using rules defined on Windows
/// platform. See this
/// [doc](https://docs.microsoft.com/en-us/previous-versions/17w5ykft(v=vs.85))
/// for more.
pub fn win_argv( cmdline: &str ) -> String {
    let mut argv = String::new();

    enum Status {
        Backslashes( usize ),
        Normal,
        Spaces,
    }

    let mut arg = String::new();
    let mut status = Status::Normal;
    let mut quoting = false;

    for ch in cmdline.chars() {
        match status {
            Status::Backslashes( ref mut count ) => match ch {
                '\\' => *count += 1,
                '"' => {
                    for _ in 0..(*count/2) {
                        arg.push('\\');
                    }
                    if *count%2 == 0 {
                        quoting = true;
                    } else {
                        arg.push('"');
                    }
                    status = Status::Normal;
                },
                _ => {
                    for _ in 0..*count {
                        arg.push('\\');
                    }
                    arg.push( ch );
                    status = Status::Normal;
                },
            },
            Status::Normal => match ch {
                '"' => quoting = !quoting,
                ' '| '\t' => {
                    if quoting {
                        arg.push( ch );
                    } else {
                        status = Status::Spaces;
                        argv.push_str( &arg );
                        arg.clear();
                        argv.push('\0');
                    }
                },
                '\\' => status = Status::Backslashes( 1 ),
                _ => arg.push( ch ),
            },
            Status::Spaces => match ch {
                '"' => {
                    status = Status::Normal;
                    quoting = true;
                },
                '\\' => status = Status::Backslashes( 1 ),
                ' ' | '\t' => (),
                _ => {
                    status = Status::Normal;
                    arg.push( ch );
                },
            },
        }
    }

    if !arg.is_empty() {
        argv.push_str( &arg );
        argv.push('\0');
    }

    argv
}

#[cfg( test )]
mod tests {
    #[cfg( target_os = "windows" )]
    use crate::windows::from_utf16_file;

    use std::{
        env,
        path::{Path, PathBuf},
    };

    use super::*;

    #[cfg( target_os = "freebsd" )]
    const JSON: &'static str = r#"{
        "process-information": {
            "process": [
                {
                    "pid": "1004",
                    "ppid": "1002",
                    "command": "alphabet",
                    "arguments": "alphabet --alpha 0 --beta 1 --gamma 2"
                },
                {
                    "pid": "1005",
                    "ppid": "1001",
                    "command": "cargo",
                    "arguments": "cargo build --no-default-features --featurues \"nigthly no_std\""
                },
                {
                    "pid": "1002",
                    "ppid": "1001",
                    "command": "cargo",
                    "arguments": "cargo check"
                },
                {
                    "pid": "1003",
                    "ppid": "1002",
                    "command": "foo",
                    "arguments": "foo -bar -baz"
                },

                {
                    "pid": "1006",
                    "ppid": "1005",
                    "command": "ping",
                    "arguments": "ping 192.168.1.1"
                },
                {
                    "pid": "1007",
                    "ppid": "1005",
                    "command": "ps",
                    "arguments": "ps aux"
                },
                {
                    "pid": "1001",
                    "ppid": "999",
                    "command": "tcsh",
                    "arguments": "-tcsh (tcsh)"
                }
            ]
        }
}"#;

    #[cfg( unix )]
    const TABLE: &'static str = r#" PID PPID COMMAND  COMMAND
1004 1002 alphabet alphabet --alpha 0 --beta 1 --gamma 2
1005 1001 cargo    cargo build --no-default-features --featurues "nigthly no_std"
1002 1001 cargo    cargo check
1003 1002 foo      foo -bar -baz
1006 1005 ping     ping 192.168.1.1
1007 1005 ps       ps aux
1001  999 tcsh     -tcsh (tcsh)"#;

    #[cfg( unix )]
    fn fake_procfs() -> PathBuf {
        let manifest_dir = env::var( "CARGO_MANIFEST_DIR" ).unwrap();

        #[cfg( target_os = "freebsd" )]
        let path = Path::new( &manifest_dir ).join( "test/freebsd/proc" );

        #[cfg( target_os = "linux" )]
        let path = Path::new( &manifest_dir ).join( "test/linux/proc" );

        path
    }

    #[cfg( target_os = "windows" )]
    fn fake_wmic_utf16_file_path() -> PathBuf {
        let manifest_dir = env::var( "CARGO_MANIFEST_DIR" ).unwrap();
        Path::new( &manifest_dir ).join( "test" ).join("windows").join( "pals.output" )
    }

    #[test]
    fn ps() {
        #[cfg( target_os = "freebsd" )]
        assert!( pals_from_ps_json().is_ok() );

        #[cfg( unix )]
        assert!( pals_from_ps_ww().is_ok() );
    }

    #[test]
    fn sort() {
        #[cfg( target_os = "freebsd" )]
        (|| {
            let proc_list = from_json( JSON ).unwrap();
            assert_eq!( proc_list.procs().map( |proc| proc.pid.0 ).collect::<Vec<_>>(),
                vec![ 1001, 1002, 1003, 1004, 1005, 1006, 1007 ]);
        })();

        #[cfg( unix )]
        (|| {
            let proc_list = from_table( TABLE ).unwrap();
            assert_eq!( proc_list.procs().map( |proc| proc.pid.0 ).collect::<Vec<_>>(),
                vec![ 1001, 1002, 1003, 1004, 1005, 1006, 1007 ]);
        })();

        #[cfg( any( target_os = "freebsd", target_os = "linux" ))]
        (|| {
            let proc_list = pals_from_procfs( &fake_procfs() ).unwrap();
            assert_eq!( proc_list.procs().map( |proc| proc.pid.0 ).collect::<Vec<_>>(),
                vec![ 1001, 1002, 1003, 1004, 1005, 1006, 1007 ]);
        })();

        #[cfg( target_os = "windows" )]
        (|| {
            let path = fake_wmic_utf16_file_path();
            let proc_list = from_utf16_file( &path ).unwrap();
            assert_eq!( proc_list.procs().map( |proc| proc.pid.0 ).collect::<Vec<_>>(),
                vec![ 1001, 1002, 1003, 1004, 1005, 1006, 1007 ]);
        })();
    }

    #[test]
    fn bfs() {
        #[cfg( target_os = "freebsd" )]
        (|| {
            let proc_list = from_json( JSON ).unwrap();
            let visits = proc_list.bfs().iter
                .map( |visit| (visit.data.pid.0, visit.size.degree, visit.size.descendants) )
                .collect::<Vec<_>>();
            assert_eq!( visits, vec![
                (1001, 2, 6),
                (1005, 2, 2),
                (1002, 2, 2),
                (1007, 0, 0),
                (1006, 0, 0),
                (1004, 0, 0),
                (1003, 0, 0),
            ]);
        })();

        #[cfg( unix )]
        (|| {
            let proc_list = from_table( TABLE ).unwrap();
            let visits = proc_list.bfs().iter
                .map( |visit| (visit.data.pid.0, visit.size.degree, visit.size.descendants) )
                .collect::<Vec<_>>();
            assert_eq!( visits, vec![
                (1001, 2, 6),
                (1005, 2, 2),
                (1002, 2, 2),
                (1007, 0, 0),
                (1006, 0, 0),
                (1004, 0, 0),
                (1003, 0, 0),
            ]);
        })();

        #[cfg( any( target_os = "freebsd", target_os = "linux" ))]
        (|| {
            let proc_list = pals_from_procfs( &fake_procfs() ).unwrap();
            let visits = proc_list.bfs().iter
                .map( |visit| (visit.data.pid.0, visit.size.degree, visit.size.descendants) )
                .collect::<Vec<_>>();
            assert_eq!( visits, vec![
                (1001, 2, 6),
                (1005, 2, 2),
                (1002, 2, 2),
                (1007, 0, 0),
                (1006, 0, 0),
                (1004, 0, 0),
                (1003, 0, 0),
            ]);
        })();

        #[cfg( target_os = "windows" )]
        (|| {
            let path = fake_wmic_utf16_file_path();
            let proc_list = from_utf16_file( &path ).unwrap();
            let visits = proc_list.bfs().iter
                .map( |visit| (visit.data.pid.0, visit.size.degree, visit.size.descendants) )
                .collect::<Vec<_>>();
            assert_eq!( visits, vec![
                (1001, 2, 6),
                (1005, 2, 2),
                (1002, 2, 2),
                (1007, 0, 0),
                (1006, 0, 0),
                (1004, 0, 0),
                (1003, 0, 0),
            ]);
        })();
    }

    #[test]
    fn forest() {
        macro_rules! process {
            ($pid:expr, $ppid:expr, $comm:expr, $args:expr) => {
                Process{ pid: Pid($pid), ppid: Pid($ppid), command: $comm.to_owned(), arguments: $args.to_owned(), }
            }
        }

        let p1 = &process!( 1001,  999, "tcsh"    , "-tcsh\0(tcsh)\0"                                                    );
        let p5 = &process!( 1005, 1001, "cargo"   , "cargo\0build\0--no-default-features\0--featurues\0nigthly no_std\0" );
        let p2 = &process!( 1002, 1001, "cargo"   , "cargo\0check\0"                                                     );
        let p7 = &process!( 1007, 1005, "ps"      , "ps\0aux\0"                                                          );
        let p6 = &process!( 1006, 1005, "ping"    , "ping\0192.168.1.1\0"                                                );
        let p4 = &process!( 1004, 1002, "alphabet", "alphabet\0--alpha\00\0--beta\01\0--gamma\02\0"                      );
        let p3 = &process!( 1003, 1002, "foo"     , "foo\0-bar\0-baz\0"                                                  );

        #[cfg( target_os = "freebsd" )]
        (|| {
            let proc_list = from_json( JSON ).unwrap();
            let bfs = proc_list.bfs();
            let forest = trees::Forest::<&Process>::from( bfs );
            let expected = trees::Forest::<&Process>::from_tuple((
                (p1, (p5, p7, p6), (p2, p4, p3), ),
            ));
            assert_eq!( forest, expected );
        })();

        #[cfg( unix )]
        (|| {
            let proc_list = from_table( TABLE ).unwrap();
            let bfs = proc_list.bfs();
            let forest = trees::Forest::<&Process>::from( bfs );
            let expected = trees::Forest::<&Process>::from_tuple((
                (p1, (p5, p7, p6), (p2, p4, p3), ),
            ));
            assert_eq!( forest, expected );
        })();

        #[cfg( any( target_os = "freebsd", target_os = "linux" ))]
        (|| {
            let proc_list = pals_from_procfs( &fake_procfs() ).unwrap();
            let bfs = proc_list.bfs();
            let forest = trees::Forest::<&Process>::from( bfs );
            let expected = trees::Forest::<&Process>::from_tuple((
                (p1, (p5, p7, p6), (p2, p4, p3), ),
            ));
            assert_eq!( forest, expected );
        })();

        #[cfg( target_os = "windows" )]
        (|| {
            let path = fake_wmic_utf16_file_path();
            let proc_list = from_utf16_file( &path ).unwrap();
            let bfs = proc_list.bfs();
            let forest = trees::Forest::<&Process>::from( bfs );
            let expected = trees::Forest::<&Process>::from_tuple((
                (p1, (p5, p7, p6), (p2, p4, p3), ),
            ));
            assert_eq!( forest, expected );
        })();
    }

    #[test]
    fn win_argv_works() {
        assert_eq!( argv_iter( &win_argv( r#""abc" d e"#       )).collect::<Vec<_>>(), vec![  "abc"    , "d"    , "e" ]);
        assert_eq!( argv_iter( &win_argv( r#"a\\\b d"e f"g h"# )).collect::<Vec<_>>(), vec![r#"a\\\b"# , "de fg", "h" ]);
        assert_eq!( argv_iter( &win_argv( r#"a\\\"b c d"#      )).collect::<Vec<_>>(), vec![r#"a\"b"#  , "c"    , "d" ]);
        assert_eq!( argv_iter( &win_argv( r#"a\\\\"b c" d e"#  )).collect::<Vec<_>>(), vec![r#"a\\b c"#, "d"    , "e" ]);
    }

    #[test]
    fn display() {

        #[cfg( any( target_os = "freebsd", target_os = "linux" ))]
        {
            let proc_list = pals_from_procfs( &fake_procfs() ).unwrap();
            assert_eq!( proc_list.to_string(),
r#"[{cmd: "tcsh", pid:1001, args:[ "-tcsh", "(tcsh)" ], subs:[
  {cmd: "cargo", pid:1005, args:[ "cargo", "build", "--no-default-features", "--featurues", "nigthly no_std" ], subs:[
   {cmd: "ps", pid:1007, args:[ "ps", "aux" ]},
   {cmd: "ping", pid:1006, args:[ "ping", "192.168.1.1" ]}]},
  {cmd: "cargo", pid:1002, args:[ "cargo", "check" ], subs:[
   {cmd: "alphabet", pid:1004, args:[ "alphabet", "--alpha", "0", "--beta", "1", "--gamma", "2" ]},
   {cmd: "foo", pid:1003, args:[ "foo", "-bar", "-baz" ]}]}]}]"# );
        }
    }
}
