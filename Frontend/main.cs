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

using System;
using System.IO;
using Scripting;

namespace NetLisp.TextFrontend
{

public sealed class NetLispTextFrontend : Scripting.TextFrontend
{ public NetLispTextFrontend(TextReader input, TextWriter output) : base(input, output) { }

  public static int Main(string[] args)
  { Options.Current.Language = NetLisp.Backend.LispLanguage.Instance;
    return new NetLispTextFrontend(Console.In, Console.Out).DoMain(args);
  }
  
  protected override string GetSnippet()
  { string code = null;
    do
    { int parens = 0;
      do
      { Output.Write(code==null ? ">>> " : "... ");
        string line = Input.ReadLine();
        if(line==null) return null;
        for(int i=0; i<line.Length; i++) // FIXME: this doesn't handle [] or parentheses inside string/character defs
          if(line[i]=='(') parens++;
          else if(line[i]==')') parens--;
        code += line + "\n";
      } while(parens>0);
    } while(code.Trim().Length==0);
    return code;
  }
  
  protected override void ShowBanner()
  { Output.WriteLine("NetLisp, a scheme-like language for the .NET platform");
    Output.WriteLine("Copyright Adam Milazzo 2005-2006. http://www.adammil.net");
  }

  protected override void ShowUsage()
  { ShowBanner();
    Output.WriteLine("usage: netlisp [option] ... [file | - | --] [arg] ...");
    Output.WriteLine();
    ShowCompilationOptions();
    Output.WriteLine();
    Output.WriteLine("Other arguments:");
    Output.WriteLine("file               Read script file");
    Output.WriteLine("-                  Read standard input");
    Output.WriteLine("--                 Enter interactive mode");
    Output.WriteLine("arg ...            Arguments to store in sys.argv[1:]");
    Output.WriteLine();
    Output.WriteLine("Environment variables:");
    Output.WriteLine("NETLISP_LIB_PATH   A path to prefix to the module search path");
  }
}

} // namespace NetLisp.Frontend