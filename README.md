# slap - Slackware Package Explorer

What it does until now:

* Supports slackpkg and [slackpkg+](http://slakfinder.org/slackpkg+.html)
* Loads all package information into binary tree
* Search by using regular expressions (PCRE)
* Search package descriptions too
* Select list to search based on repository and installed/uninstalled status

Use `slap -h` for help

A static-linked version of executables is [here](https://github.com/nereusx/slap/raw/master/release/binaries.zip)

I **NEED an icon** for the application, I'll appreciate any help.

I usually write in C and/or C++ but I wrote this in Free Pascal (Lazarus) since the VCL is the best library for GUI.

## Install

```
git clone https://github.com/nereusx/slap.git
cd slap
make pack
make install
```

## Screenshots

![startup](https://github.com/nereusx/slap/blob/master/images/lazslap-1.png)

![using filters](https://github.com/nereusx/slap/blob/master/images/lazslap-2.png)

![terminal](https://github.com/nereusx/slap/blob/master/images/slap-1.png)
