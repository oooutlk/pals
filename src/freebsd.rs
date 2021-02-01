use std::{
    fs,
    path::Path,
};

use super::*;

#[derive( Debug, Deserialize )]
pub(crate) struct PsOutput {
    #[serde( rename = "process-information" )]
    pub(crate) proc_list : ProcList,
}

struct PidString;

impl<'de> de::Visitor<'de> for PidString {
    type Value = Pid;

    fn expecting( &self, formatter: &mut fmt::Formatter ) -> fmt::Result {
        formatter.write_str( "an integer between 0 and 2^32" )
    }

    fn visit_str<E: de::Error>( self, value: &str ) -> Result<Self::Value, E> {
        u32::from_str_radix( value, 10 )
            .map( |v| Pid( v ))
            .map_err( |e| E::custom( e.to_string() ))
    }
}

pub(super) fn parse_id<'de, D>( deserializer: D ) -> Result<Pid, D::Error>
    where D: de::Deserializer<'de>
{
    Ok( deserializer.deserialize_str( PidString )? )
}

pub(super) fn pals_from_procfs( path: impl AsRef<Path> ) -> anyhow::Result<ProcList> {
    let mut procs = Vec::new();
    let dirs = fs::read_dir( path )?;
    for dir in dirs {
        if let Ok( entry ) = dir {
            let entry_path = entry.path();

            match fs::read( entry_path.join( "status" )) {
                Ok( status ) => {
                    let status = String::from_utf8( status )?;
                    let mut three_values = status.split_ascii_whitespace();
                    let command   = unescape( three_values.next().context( "missing command in status file" )? )?;
                    let pid       = Pid( u32::from_str_radix( three_values.next().context( "missing pid in status file" )?, 10 )? );
                    let ppid      = Pid( u32::from_str_radix( three_values.next().context( "missing ppid in status file" )?, 10 )? );
                    let arguments = str::from_utf8( &fs::read( entry_path.join( "cmdline" ))? )?.trim_end().to_owned();

                    procs.push( ProcNode {
                        process : Process{ pid, ppid, command, arguments },
                        link    : Link::default(),
                    });
                },
                Err( _ ) => continue,
            }
        }
    }

    if procs.is_empty() {
        return Err( anyhow!( "/proc is empty" ));
    }

    build_tree( ProcList{ procs, root: Link::default() }, ArgvStatus::Splitted )
}

fn unescape( input: &str ) -> anyhow::Result<String> {
    let mut output = String::with_capacity( input.len() );

    let mut chars = input.chars();
    let mut unescaping = Vec::<u8>::new();

    while let Some( ch ) = chars.next() {
        if ch == '\\' {
            output.push_str( &parse_octlets( &mut unescaping, &mut chars )? );
        } else {
            output.push( ch );
        }
    }

    Ok( output )
}

fn parse_octlets( unescaping: &mut Vec<u8>, chars: &mut impl Iterator<Item=char> ) -> anyhow::Result<String> {
    loop {
        let chars_next = chars.next();
        match chars_next {
            Some( '3' ) => {
                for _ in 0..7 {
                    match chars.next() {
                        Some('7') => (),
                        _ => return Err( anyhow!("bad escape of command name in status file: invalid unicode.")),
                    }
                }
                if let Some( ch ) = chars.next() {
                    let init = match ch {
                        '5' => 1 << 6,
                        '6' => 2 << 6,
                        '7' => 3 << 6,
                        _ => return Err( anyhow!("bad escape of command name in status file: invalid octlet {:?}.", ch )),
                    };
                    unescaping.push( u8_from_octlets( init, chars )? );
                    match str::from_utf8( unescaping ).map( ToOwned::to_owned ) {
                        Ok( s ) => {
                            unescaping.clear();
                            return Ok( s );
                        },
                        Err(_) => match chars.next() {
                            Some( '\\' ) => continue,
                            _ => return Err( anyhow!("bad escape of command name in status file: invalid utf8 {:?}.", unescaping )),
                        },
                    }
                } else {
                    return Err( anyhow!("bad escape of command name in status file: incomplete octlets."));
                }
            },
            None => return Err( anyhow!("bad escape of command name in status file: ends with backslash.")),
            Some( ch@_ ) => {
                let init = match ch {
                    '0' => 0,
                    '1' => 1 << 6,
                    _ => return Err( anyhow!("bad escape of command name in status file: invalid octlet {:?}.", ch )),
                };
                return Ok( str::from_utf8( &[ u8_from_octlets( init, chars )? ])?.to_owned() );
            },
        }
    }
}

fn u8_from_octlets( init: u8, chars: &mut impl Iterator<Item=char> ) -> anyhow::Result<u8> {
    let mut result = init;
    for i in 0..2 {
        match chars.next() {
            Some( ch@'0'..='7' ) => result += ( (ch as u8) - ('0' as u8) ) << (3*(1-i)),
            ch@_ => return Err( anyhow!("bad escape of command name in status file: invalid octlet {:?}.", ch)),
        }
    }
    Ok( result )
}
