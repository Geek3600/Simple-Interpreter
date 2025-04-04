; ModuleID = 'myScalaCompiler'
source_filename = "myScalaCompiler"

define i32 @add(i32 %a, i32 %b) {
entry:
  %addtmp = add i32 %a, %b
  ret i32 %addtmp
}
