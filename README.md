# geosam <a href="https://walker-data.com/geosam/"><img src="man/figures/logo.png" align="right" height="120" alt="geosam website" /></a>

The **geosam** R package brings Meta's [Segment Anything Model 3 (SAM3)](https://ai.meta.com/sam2/) to R for detecting objects in satellite imagery and photos. Describe what you're looking for in plain text—no training data or model fine-tuning required. The package is inspired by the Python package [segment-geospatial](https://samgeo.gishub.org/) by Qiusheng Wu, and aims to bring similar functionality to R users.

Install the development version from GitHub:

```r
remotes::install_github("walkerke/geosam")
```

Then set up the Python environment:

```r
library(geosam)
geosam_install()
```

You'll need a [HuggingFace account](https://huggingface.co/) with access to the [SAM3 model](https://huggingface.co/facebook/sam3).

Read through these vignettes to learn how to use the package:

- [Getting started with __geosam__](https://walker-data.com/geosam/articles/getting-started.html)

- [Satellite imagery detection](https://walker-data.com/geosam/articles/satellite-detection.html)

- [Regular image detection](https://walker-data.com/geosam/articles/image-detection.html)

- [Interactive workflows](https://walker-data.com/geosam/articles/interactive.html)

## Support and how to learn more

If you find this project useful in your work and would like to ensure continued development of the package, you can provide support in the following ways:

* [Chip in some funds to support package development via PayPal](https://www.paypal.com/paypalme/walkerdata/);
* Set up a consulting engagement or workshop through Walker Data to help you implement __geosam__ in your project. Send a note to <kyle@walker-data.com> if you are interested;
* File an issue - or even better, a pull request - at https://github.com/walkerke/geosam/issues.

To stay on top of package updates / new features and to get information about trainings, [be sure to sign up for the Walker Data mailing list here](https://walker-data.us15.list-manage.com/subscribe?u=1829a68a5eda3d301119fdcd6&id=c4a53d2961).
