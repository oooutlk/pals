use super::*;

use trees::bfs::{BfsForest, Split, Splitted};

impl ProcList {
    /// Returns processes in breadth first search. This method helps to convert
    /// `ProcList` into `trees::Forest`.
    ///
    ///  See [trees](https://crates.io/crates/trees) for more.
    pub fn bfs<'a, 's:'a>( &'s self ) -> BfsForest<Splitted<Proc<'a>>> {
        BfsForest::from( self.children(), self.root.size )
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

#[cfg( test )]
mod tests {
    use super::*;
    use super::tests::*;

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
}
