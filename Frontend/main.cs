/*
NetLisp is the reference implementation for a language similar to
Scheme, also called NetLisp. This implementation is both interpreted
and compiled, targetting the Microsoft .NET Framework.

http://www.adammil.net/
Copyright (C) 2005 Adam Milazzo

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
using System.Collections;
using System.IO;
using System.Reflection.Emit;
using Scripting;
using NetLisp.Backend;

namespace NetLisp.Frontend
{

public class App
{ static void DoInteractive() // TODO: provide this functionality in the standard library as a repl() function or something
  { TopLevel.Current = new TopLevel();
    MemberContainer builtins = Options.Current.Language.Builtins;
    if(builtins!=null) builtins.Import(TopLevel.Current);
    AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);

    while(true)
    { string code = null;
      int  parens = 0;
      do
      { Console.Write(code==null ? ">>> " : "... ");
        string line = Console.ReadLine();
        if(line==null) return;
        for(int i=0; i<line.Length; i++) // FIXME: this doesn't handle [] or parentheses inside string/character defs
          if(line[i]=='(') parens++;
          else if(line[i]==')') parens--;
        code += line + "\n";
      } while(parens>0);

      if(code.Trim().Length==0) continue;
      try
      { Node node = Options.Current.Language.Parse("<interactive>", code);
        Console.WriteLine(Ops.Repr(Compiled ? SnippetMaker.Generate(Scripting.AST.CreateCompiled(node)).Run(null)
                                            : Scripting.AST.Create(node).Evaluate()));
      }
      catch(Exception e) { Console.WriteLine("ERROR: "+e.ToString()); }
    }
  }

  static int Main(string[] args)
  { Options.Current.Debug = true;
    Options.Current.Optimize = OptimizeType.Speed;
    Options.Current.Language = LispLanguage.Instance;

    string filename=null, outfile=null;
    PEFileKinds exeType = PEFileKinds.ConsoleApplication;
    ArrayList errors = new ArrayList();

    for(int i=0; i<args.Length; i++)
    { string arg = args[i];
      if((arg[0]=='-' || arg[0]=='/') && arg.Length!=1 && arg!="--")
      { string value;
        { int index = arg.IndexOfAny(new char[] { ':', '-', '+' }, 1);
          if(index==-1) { value=""; arg=arg.Substring(1); }
          else
          { value = arg[index]==':' ? arg.Substring(index+1) : arg[index].ToString();
            arg=arg.Substring(1, index-1);
          }
        }
        try
        { switch(arg)
          { case "banner": Banner=IsTrue(value); break;
            case "compiled": Compiled=IsTrue(value); break;
            case "debug": Options.Current.Debug=IsTrue(value); break;
            case "?": case "help": case "-help": Usage(); return 0;
            // TODO: implement this. case "lib": sys.path.insert(0, value); break;
            case "o": case "optimize":
              if(value=="speed") Options.Current.Optimize = OptimizeType.Speed;
              else if(value=="size") Options.Current.Optimize = OptimizeType.Size;
              else Options.Current.Optimize = value!="none" && IsTrue(value) ? OptimizeType.Speed : OptimizeType.None;
              break;
            case "out": outfile=value; break;
            case "snippets": WriteSnippets=IsTrue(value); break;
            case "t": case "target":
              switch(value)
              { case "exe": exeType = PEFileKinds.ConsoleApplication; break;
                case "winexe": exeType = PEFileKinds.WindowApplication; break;
                case "dll": case "lib": case "library":
                  exeType = PEFileKinds.Dll; break;
                default: errors.Add("Unknown value for -type: "+value); break;
              }
              break;
            default: errors.Add("Unknown argument: "+arg); break;
          }
        }
        catch { errors.Add("Invalid value for -"+arg+": "+value); }
      }
      else if(arg=="/") errors.Add("Invalid switch: /");
      else
      { // TODO: implement this
        if(arg!="--") { /*sys.argv.append(arg);*/ filename=arg; }
        /*else sys.argv.append("");
        for(i++; i<args.Length; i++) sys.argv.append(args[i]);
        */
      }
    }

    if(errors.Count!=0)
    { foreach(string error in errors) Console.Error.WriteLine("ERROR: "+error);
      Usage();
      return 1;
    }
    errors = null;

    if(Banner && (filename==null || outfile!=null)) ShowBanner();

    if(filename==null)
      try { DoInteractive(); }
      finally { if(WriteSnippets) SnippetMaker.DumpAssembly(); }
    else
    { bool stdin = filename=="-";
      string basename = stdin ? "main" : Path.GetFileNameWithoutExtension(filename);

      try
      { if(outfile!=null) Console.Error.WriteLine("Parsing {0}...", stdin ? "standard input" : filename);

        Node node;
        if(stdin) node = Options.Current.Language.Parse("<stdin>", Console.In);
        else
        { // TODO: implement this. sys.path[0] = Path.GetDirectoryName(Path.GetFullPath(filename));
          node = Options.Current.Language.ParseFile(filename);
        }

        if(outfile!=null)
        { Console.Error.WriteLine("Compiling...");
          ModuleGenerator.Generate(basename, outfile, Scripting.AST.CreateCompiled(Backend.AST.Create(node)),
                                   exeType); // FIXME: if snippetmaker is used (eg, if eval is called), the snippets will be lost
          Console.Error.WriteLine("Successfully wrote "+outfile);
        }
        else
          try
          { TopLevel.Current = new TopLevel();
            MemberContainer builtins = Options.Current.Language.Builtins;
            if(builtins!=null) builtins.Import(TopLevel.Current);
            SnippetMaker.Generate(Scripting.AST.CreateCompiled(Backend.AST.Create(node)), basename).Run(null);
          }
          finally { if(WriteSnippets) SnippetMaker.DumpAssembly(); }
      }
      catch(Exception e)
      { Console.Error.WriteLine("Errors occurred during compilation:");
        if(e is SyntaxErrorException) Console.Error.WriteLine(e.Message);
        else Console.Error.WriteLine(e);
        return 1;
      }
    }

    return 0;
  }

  static string GenerateName(string name, PEFileKinds type)
  { string ext = type==PEFileKinds.Dll ? ".dll" : ".exe";
    string baseName = Path.GetDirectoryName(name);
    if(baseName!="") baseName += Path.DirectorySeparatorChar;
    baseName += Path.GetFileNameWithoutExtension(name);

    name = baseName + ext;
    for(int i=0; File.Exists(name); i++) name = baseName + i.ToString() + ext;
    return name;
  }

  static bool IsTrue(string value)
  { switch(value.ToLower())
    { case "-": case "0": case "no": case "off": case "false": return false;
      case "+": case "1": case "yes": case "on": case "true": return true;
      default: throw new ArgumentException();
    }
  }
  
  static void ShowBanner()
  { Console.Error.WriteLine("NetLisp, a scheme-like language for the .NET platform");
    Console.Error.WriteLine("Copyright Adam Milazzo 2005. http://www.adammil.net");
  }

  static void UnhandledException(object sender, UnhandledExceptionEventArgs e)
  { Console.WriteLine("An unhandled exception occurred{0}:",
                      e.IsTerminating ? " and the application must terminate" : "");
    Console.WriteLine(e.ExceptionObject);
  }

  static void Usage()
  { ShowBanner();
    Console.WriteLine("usage: netlisp [option] ... [file | - | --] [arg] ...");
    Console.WriteLine();
    Console.WriteLine("Options:");
    Console.WriteLine("-banner[-|+]       Display copyright banner");
    Console.WriteLine("-compiled[-|+]     Enable compiled code (default=on)");
    Console.WriteLine("-debug[-|+]        Emit debugging information (default=on)");
    Console.WriteLine("-help              Show this message");
    Console.WriteLine("-lib:<path>        Specify additional library paths");
    Console.WriteLine("-o[ptimize][-|+]   Enable optimizations (default=on)");
    Console.WriteLine("-out:<file>        Compile and save the output (overrides -compiled)");
    Console.WriteLine("-snippets[-|+]     Save the snippets .dll");
    Console.WriteLine("-t[arget]:exe      Build a console application (used with -out)");
    Console.WriteLine("-t[arget]:library  Build a library (used with -out)");
    Console.WriteLine("-t[arget]:winexe   Build a windowed application (used with -out)");
    Console.WriteLine();
    Console.WriteLine("Other arguments:");
    Console.WriteLine("file               Read script file");
    Console.WriteLine("-                  Read standard input");
    Console.WriteLine("--                 Enter interactive mode");
    Console.WriteLine("arg ...            Arguments to store in sys.argv[1:]");
    Console.WriteLine();
    Console.WriteLine("Environment variables:");
    Console.WriteLine("NETLISP_LIB_PATH   A path to prefix to the module search path");
  }

  static bool Banner=true, Compiled=true, WriteSnippets;
}

} // namespace NetLisp.Frontend