# imageCompressor

This project's goal was to make an image compressor fully in haskell using clustering.
This was made on Fedora linux using Haskell and stack to build.

Each colors of the image are grouped into "Clusters", then, based on the number of colors you want, it choose the most prevalent colors to keep, making a compressed image.

How to compile:

```
make
```

You can test it doing this:

```
./ic_tools_fedora36/convertImg your_image > converted_img.txt
./imageCompressor -n NbColors -l convergenceNb (for example 0.08) -f converted_img.txt > compressed_img.txt
./ic_tools_fedora36/xpmImg compressed_img.txt > compressed.xpm
```

And Voilà ! You have a compressed image :D

It was a fun project to do, a bit hard because doing this type of algorithm in Haskell is hard, but I like the result !
