--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with GNATCOLL.Traces;

--  Prettier_Ada root package
package Prettier_Ada is

   Gnatfmt_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("PRETTIER_ADA", GNATCOLL.Traces.Off);

   Version    : constant String := "debug";
   Build_Date : constant String := "debug";

end Prettier_Ada;
