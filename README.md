# HW2-Statistical-Learning
Project on classification on songs
Well. . . actually it is, and you can even listen to some masterpiece of this (inconceivably) overlooked treasure of the world music. This said, your task here is to classify a bunch of 30 seconds long song snippets of 5 – definitively less exotic – genres. Here’s the details. . .


3. Extract some relevant features for classification
Now that you know how to import the sound wave, it’s time to extract some relevant features for classification. Typical examples are autocorrelation or spectral quantities derived from a suitably parametrized Short–time Fourier transform, but you may go “ballistic” and somewhat creative.
In base R there are already interesting functions like spectrum(), and plot.spec() but, beside wrassp, there’re many other packages that may come in handy. For example:
1. signal: includes filter generation utilities, filtering functions, resampling routines, and visualization of filter models.
2. tuneR: analyze music and speech, extract features like MFCCs, handle wave files and their representation in various ways, read mp3, etc.
