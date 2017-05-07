# **DotNet Patcher v4** #

# Description

DNP is a Obfuscator/Packer which use MonoCecil library for .NET applications !
It doesn't support WPF .exe !

# Screenshot

![DotNetPatcher.png](http://i.imgur.com/NtlPr31.gif)


# Features

* English UI language only
* Displays selected .exe informations (assembly name, Version, TargetRuntime, TargetCPU, SubSystemType)
* Selecting encoding chars type : Alphabetic, Dots, Invisible, Chinese, Japanese, Greek, Flowing) 
* Renaming : Namespaces, Types, Methods, Properties, Fields, Custom Attributes, Events, Parameters, ..... 
* Dependencies merging/embedding (encrypt and/or compress)
* Displays number of renamed members
* Exclusionary rules Manager
* Anti-IlDasm, Anti-Tamper, Anti-Debug, Anti-Dumping
* "System-Reflection" methods calls detection
* Public calls hidding 
* Pinvoke calls hidding (Use with caution)
* Strings encryption
* Numerics encryption
* Constants encoding
* Booleans encoding
* Invalid Metadatas (Use with caution)
* Mild calls
* Stackflow (Use with caution)
* Fake attributes injection
* Invalid opcodes injection
* Packer with LZX compression and encryption


# Prerequisites

* All Windows OS
* DotNet Framework 4.0
* The binary doesn't require installation


# WebSite

* [http://3dotdevcoder.blogspot.fr/](http://3dotdevcoder.blogspot.fr/)


# Credits

* 0xd4d : for [Dnlib Pe module] (https://github.com/0xd4d/dnlib) library
* jbevains : for [MonoCecil](https://github.com/jbevain/cecil) library
* Yck1509 : for [Confuser](https://confuser.codeplex.com/) project 
* Daniel Doubrovkine : for his ResourceLib library [Vestris](https://github.com/dblock/resourcelib)
* Markhor : For his Sevenzip library [Sevenzip](https://sevenzipsharp.codeplex.com/)
* Xertz : for his [Login GDI+ theme](http://xertzproductions.weebly.com/login-gdi-theme.html) which I modified a little bit
* Paupino : for some useful functions from his open source project : [NCloak](https://github.com/paupino/ncloak/)
* Mirhabi : for his IconInjector class


# Copyright

Copyright © 3DotDev 2008-2017


# Licence

[MIT/X11](http://en.wikipedia.org/wiki/MIT_License)