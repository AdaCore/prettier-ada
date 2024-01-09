--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GNATCOLL.Traces;

package Document_Formatter is

   Document_Formatter_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("DOCUMENT_FORMATTER", GNATCOLL.Traces.Off);
   Version : constant String := "debug";

end Document_Formatter;
