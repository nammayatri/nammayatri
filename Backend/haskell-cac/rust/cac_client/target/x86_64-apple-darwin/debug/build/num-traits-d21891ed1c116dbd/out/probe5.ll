; ModuleID = 'probe5.696845693e51eac9-cgu.0'
source_filename = "probe5.696845693e51eac9-cgu.0"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.12.0"

; core::f64::<impl f64>::is_subnormal
; Function Attrs: inlinehint uwtable
define internal zeroext i1 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$12is_subnormal17hc86862c4aa667be2E"(double %self) unnamed_addr #0 {
start:
  %_2 = alloca i8, align 1
; call core::f64::<impl f64>::classify
  %0 = call i8 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$8classify17hdd6b8ac18bb50ae1E"(double %self), !range !2
  store i8 %0, ptr %_2, align 1
  %1 = load i8, ptr %_2, align 1, !range !2, !noundef !3
  %_3 = zext i8 %1 to i64
  %_0 = icmp eq i64 %_3, 3
  ret i1 %_0
}

; probe5::probe
; Function Attrs: uwtable
define void @_ZN6probe55probe17h3441e04c685d3c26E() unnamed_addr #1 {
start:
; call core::f64::<impl f64>::is_subnormal
  %_1 = call zeroext i1 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$12is_subnormal17hc86862c4aa667be2E"(double 1.000000e+00)
  ret void
}

; core::f64::<impl f64>::classify
; Function Attrs: uwtable
declare i8 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$8classify17hdd6b8ac18bb50ae1E"(double) unnamed_addr #1

attributes #0 = { inlinehint uwtable "frame-pointer"="all" "probe-stack"="inline-asm" "target-cpu"="penryn" }
attributes #1 = { uwtable "frame-pointer"="all" "probe-stack"="inline-asm" "target-cpu"="penryn" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{!"rustc version 1.74.1 (a28077b28 2023-12-04)"}
!2 = !{i8 0, i8 5}
!3 = !{}
