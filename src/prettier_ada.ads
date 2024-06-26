--
--  Copyright (C) 2023-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GNATCOLL.Traces;

--  Prettier_Ada root package

package Prettier_Ada is

   Prettier_Ada_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("PRETTIER_ADA", GNATCOLL.Traces.Off);

   Version    : constant String := "debug";
   Build_Date : constant String := "debug";

end Prettier_Ada;
