using System.Reflection;
using System.Runtime.CompilerServices;

[assembly: AssemblyTitle("NetLisp Text Frontend")]
[assembly: AssemblyDescription("A console-based frontend for the NetLisp language.")]
#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif
[assembly: AssemblyProduct("NetLisp")]
[assembly: AssemblyCopyright("Copyright 2005, Adam Milazzo")]

[assembly: AssemblyVersion("0.1.*")]

[assembly: AssemblyKeyFile("../../../NetLisp.snk")]