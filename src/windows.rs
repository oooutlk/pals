use std::{
    env,
    fs::{self ,File},
    io::BufReader,
    path::Path,
    process::Command,
};

use super::{
    ArgvStatus,
    Pid,
    ProcList,
    ProcNode,
    Process,
    build_tree,
};

pub(crate) fn pals_from_wmic() -> anyhow::Result<ProcList> {
    let mut temp_file = env::temp_dir();
    temp_file.push( "pals.output");
    let temp_file = temp_file.to_str().unwrap();

    let result = (|| -> anyhow::Result<ProcList> {
        Command::new("wmic")
            .arg( &format!( "/OUTPUT:{}", temp_file ))
            .arg( "process" )
            .arg( "get" )
            .arg( "CommandLine,Name,ParentProcessId,ProcessId" )
            .arg( "/value" )
            .output()?;

        from_utf16_file( &temp_file )
    })();
    fs::remove_file( temp_file )?;
    result
}

pub(crate) fn from_utf16_file( p: impl AsRef<Path> ) -> anyhow::Result<ProcList> {
    let f = File::open( p.as_ref() ).unwrap();
    let r = BufReader::new( f );
    let s = utf16_reader::read_to_string( r );
    from_utf8_wmic_values( &s )
}

fn from_utf8_wmic_values( input: &str ) -> anyhow::Result<ProcList> {
    build_tree( parse( &input )?, ArgvStatus::Parsing )
}

fn parse( input: &str ) -> anyhow::Result<ProcList> {
    #[derive( Default )]
    struct _Process {
        _fields : usize,
        pid     : Option<u32>,
        ppid    : Option<u32>,
        name    : Option<String>,
        args    : Option<String>,
    }
    impl _Process {
        fn take( &mut self ) -> Option<Process> {
            if self._fields == 4 {
                self._fields = 0;
                Some( Process{
                    pid       : Pid( self.pid .take().unwrap() ),
                    ppid      : Pid( self.ppid.take().unwrap() ),
                    command   : self.name.take().unwrap(),
                    arguments : self.args.take().unwrap(),
                })
            } else {
                None
            }
        }

        fn set_pid( &mut self, v: u32 ) {
            if self.pid.is_none() {
                self._fields += 1;
            }
            self.pid = Some(v);
        }

        fn set_ppid( &mut self, v: u32 ) {
            if self.ppid.is_none() {
                self._fields += 1;
            }
            self.ppid = Some(v);
        }

        fn set_name( &mut self, v: String ) {
            if self.name.is_none() {
                self._fields += 1;
            }
            self.name = Some(v);
        }

        fn set_args( &mut self, v: String ) {
            if self.args.is_none() {
                self._fields += 1;
            }
            self.args = Some(v);
        }
    }

    let mut proc_list = ProcList::default();
    let mut process = _Process::default();
    for line in input.lines() {
        if let Some( args ) = line.strip_prefix("CommandLine=") {
            process.set_args( args.trim_end().to_owned() );
        } else if let Some( name ) = line.strip_prefix("Name=") {
            process.set_name( name.trim_end().to_owned() );
        } else if let Some( ppid ) = line.strip_prefix("ParentProcessId=") {
            process.set_ppid( u32::from_str_radix( ppid.trim_end(), 10 )? );
        } else if let Some( pid ) = line.strip_prefix("ProcessId=") {
            process.set_pid( u32::from_str_radix( pid.trim_end(), 10 )? );
        } else {
            continue;
        }
        if let Some( process ) = process.take() {
            proc_list.procs.push( ProcNode::from( process ));
        }
    }
    Ok( proc_list )
}

#[cfg( test )]
mod tests {
    use super::*;

    #[test]
    fn wmic() {
        assert!( pals_from_wmic().is_ok() );
    }
}
