# Project goal

The pals project aims at providing a binary utility and a Rust library to
generate a running _Processes' Arguments LiSt_ the portable way.

# Application interface

See [API doc](https://docs.rs/pals/0.1.1) for more.

# Binary utility

The pals command will dump running processes' name, pid, arguments and the sub
processes to form a JSON array of elements which are process trees.

A process is a JSON object composed of:

  1. cmd: Process name.

  2. pid: Process id.

  3. args: JSON array of splitted arguments, aka argv. It's optional.

  4. subs: JSON array of sub processes. It's optional.

## Example of output

```js
[{cmd: "alpha", pid:105, args:[ "alpha" ], subs:[
  {cmd: "beta", pid:107, args:[ "beta", "--help" ]},
  {cmd: "gamma", pid:106 }]},
 {cmd: "lorum", pid:102, subs:[
  {cmd: "ipsumipsum", pid:104, args:[ "ipsumipsumipsum", "--name-too-long" ]}]}]
```

Thanks to identing, the JSON output is even human readable.

# Supported platform

Supporting on Windows, Linux and FreeBSD are tested. Furthermore, pals works on
any platform that supports `ps` command, for example, macOS. However, `ps` is
NOT required on all supported platforms.

# Caveat

On some platforms, process names may be truncted to fit in the maximum length.

# License

Under Apache License 2.0 or MIT License, at your will.
