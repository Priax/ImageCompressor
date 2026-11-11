# imageCompressor

This project's goal was to make an image compressor using the "kMeans" algorithm, fully in haskell using clustering.
This was made on Fedora linux using Haskell and stack to build.

The pixels are grouped into as many "clusters" as the number of colors you want: each cluster gets the average color of its pixels (k-means, seeded with k-means++), which gives the compressed image.

The k-means runs on the distinct colors of the image weighted by their number of pixels, and its iterations use every core.

How to compile:

```
make
```

You can test it doing this:

```
./ic_tools_fedora36/convertImg your_image > converted_img.txt
./imageCompressor -n NbColors -l convergenceNb (for example 0.08) -f converted_img.txt [-s seed] > compressed_img.txt
./ic_tools_fedora36/xpmImg compressed_img.txt > compressed.xpm
```

And Voilà ! You have a compressed image :D

The optional `-s` sets the seed of the random generator, to get the same result on every run. On error (invalid arguments or input file), the program prints the reason on stderr and exits with code 84.

Example with 4 colors:

<div style="display: flex;">
    <div style="flex: 1;">
        <img src="./example/RinWitch.jpg" alt="Image of Rin Tezuka as a Witch" width="150">
    </div>
    <div style="flex: 1;">
        <img src="./example/rin.png" alt="Image of Rin Tezuka as a Witch, but compressed" width="150">
    </div>
</div>

It was a fun project to do, a bit hard because doing this type of algorithm in Haskell is hard, but I like the result !
