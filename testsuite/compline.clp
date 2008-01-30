(deffunction compare-files (?base-file ?test-file ?output)
   (open ?base-file base)
   (open ?test-file test)
   (bind ?i 0)
   (bind ?difference FALSE)
   (bind ?base-line (readline base))
   (bind ?test-line (readline test))
   (while (and (neq ?base-line EOF) (neq ?test-line EOF)) do
      (bind ?i (+ ?i 1))
      (if (neq ?base-line ?test-line) then
          (bind ?difference TRUE)
          (format ?output "   %3d: %s%n" ?i ?base-line)
          (format ?output "   %3d: %s%n" ?i ?test-line))
      (bind ?base-line (readline base))
      (bind ?test-line (readline test)))
   (if (or (neq ?base-line EOF) (neq ?test-line EOF)) then
      (bind ?difference TRUE)
      (printout ?output "   Files do not have the same # of lines" crlf))
   (if (not ?difference) then
      (format ?output "   No differences detected in %d lines.%n" ?i))
   (close base)
   (close test))