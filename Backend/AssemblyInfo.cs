/*
NetLisp is the reference implementation for a language similar to
Scheme, also called NetLisp. This implementation is both interpreted
and compiled, targetting the Microsoft .NET Framework.

http://www.adammil.net/
Copyright (C) 2005-2006 Adam Milazzo

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

using System.Reflection;
using System.Runtime.CompilerServices;

[assembly: AssemblyTitle("NetLisp Backend")]
[assembly: AssemblyDescription("The backend for the NetLisp language.")]
#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif
[assembly: AssemblyProduct("NetLisp")]
[assembly: AssemblyCopyright("Copyright 2005, Adam Milazzo")]

[assembly: AssemblyVersion("0.9.0.0")]

[assembly: InternalsVisibleTo("Scripting, PublicKey=00240000048000009400000006020000002400005253413100040000010001008fceb34723766d651592a4d0d50563755ea63a3594207a4032a92059ca196cf5c16b238b14b795667eedc04abce3978cbda8cde83d790dbc21ec5cb233ef79022c68363a53b039500399c047ab27566a08c2901319839610f9a98f380b932e57413e50b8c55356605ce400e241a4f3371b884f3abb33d22e1bc7e65ad3ce24c7")]