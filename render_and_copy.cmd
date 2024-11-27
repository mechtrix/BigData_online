rmdir docs /S /Q
md docs
quarto render
robocopy _site docs /E