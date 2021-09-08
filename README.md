# annotation-mode.el
`annotation-mode` is a minor mode for annotating regions of text documents for machine learning purposes.

# In Action

See it in action on YouTube:

[![Alt text](https://img.youtube.com/vi/CIzD3hYYXYo/0.jpg)](https://youtu.be/CIzD3hYYXYo)

# Installation

I have no idea or patience to get this onto MELPA or ELPA. However this package may be installed using [`straight`](https://github.com/raxod502/straight.el)

```
(use-package annotation-mode
	:straight
	(annotation-mode
		:type git
		:host github
		:files (:defaults "annoation-mode")
		:repo "chewxy/annotation-mode.el"))
```

# Usage

To use, simply turn on the mode: `M-x annotaion-mode`. By default, annotations are not saved to a file. To save to a  file you need to set the annotation file `M-x set-annotation-file`. The file must be a JSON file.

Select a region of text in the buffer that has the annotation-mode turned on. The minibuffer will pop up at the bottom asking what class the selected region will be. Type it in and the annotation will be made. If a annotation file was set, then the annotation will be written to the file. Otherwise it will be displayed as a message.
