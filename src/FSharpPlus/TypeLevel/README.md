`FSharpPlus.TypeLevel.fsproj` is a project for debugging and testing type-level stuffs.

* While debugging a type provider, one must rebuild the project every time a change is made to the code.
  Creating a small project just for debug greatly speeds up the process.
* Type-level tests are run by compiling it. They are enabled with compiler constant `TYPELEVEL_DEBUG` and thus will not be included in the package.

