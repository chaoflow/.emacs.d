;;; nix helper

(defun find-corresponding-nix-hash (file)
  "Given some file or directory which ultimately lies in the nix
store. Return the corresponding nix hash."
  (when file
   (let ((truename (file-truename file)))
     (when (string-match "/nix/store/\\([^/-]+\\)-" truename)
       (match-string 1 truename)))))
