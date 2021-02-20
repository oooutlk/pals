use std::{
    fs,
    path::Path,
};

use super::*;

#[cfg( target_os = "linux" )]
pub(super) fn pals_from_procfs( path: impl AsRef<Path> ) -> anyhow::Result<ProcList> {
    let mut procs = Vec::new();
    let dirs = fs::read_dir( path )?;
    for dir in dirs {
        if let Ok( entry ) = dir {
            let entry_path = entry.path();

            let stat_file = entry_path.join( "stat" );
            match fs::read( stat_file ) {
                Ok( stat ) => {
                    let stat = String::from_utf8( stat )?;

                    let first_space = stat.find(' ').context( "stat file should contain space" )?;
                    let pid = &stat[..first_space];
                    let pid = Pid( u32::from_str_radix( pid, 10 )? );

                    let first_left_paren = stat.find('(').context( "stat file should contain '('" )?;
                    let last_right_paren = stat.rfind(')').context( "stat file should contain ')'" )?;
                    let command = stat[ first_left_paren+1..last_right_paren ].to_string();

                    let ppid_start = stat[last_right_paren+2..]
                        .find(' ')
                        .context( "missing ppid in stat file" )?
                        + last_right_paren + 3;

                    let ppid_end = stat[ ppid_start.. ]
                        .find(' ')
                        .context( "missing ppid in stat file" )?
                        + ppid_start;

                    let ppid = &stat[ ppid_start..ppid_end ];
                    let ppid = Pid( u32::from_str_radix( ppid, 10 )? );

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
