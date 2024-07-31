# NUSS

NUSS (Mixed N-Grams and Unigram Sequence Segmentation) is an R package designed to segment and simplify sequences using synthetic n-grams sequences, and dynamic unigram sequence matching. This package is particularly useful for text processing and natural language processing (NLP) tasks.

## Installation

You can install the development version of NUSS from GitHub using the following commands:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install NUSS from GitHub
devtools::install_github("theogrost/NUSS")
```

## Usage

Here are some basic examples to get you started with NUSS.

### Segmenting Sequences with N-grams

You can segment sequences using the `ngrams_segmentation` function. First, create an n-grams dictionary, and then use the dictionary to segment sequences.

```r
library(NUSS)

# Segment a sequence using built-in dictionary
unigram_sequencer_segmented <- unigram_sequence_segmentation("thisisscience")
print(unigram_sequencer_segmented)

# Example text data
texts <- c(
  "this is science",
  "science is fascinating",
  "this is a scientific approach",
  "science is everywhere",
  "the beauty of science"
)

# Segment a sequence using n-grams
ngrams_dict <- ngrams_dictionary(texts)
ngrams_segmented <- ngrams_segmentation("thisisscience", ngrams_dict)
print(ngrams_segmented)

# Segment a sequence using nuss - combined function
nuss_segmented <- nuss("thisisscience", texts)
print(segmented)
```

### Advanced Usage

You can customize the segmentation process with additional parameters such as `simplify`, `omit_zero`, and `score_formula`.

```r
# Custom segmentation with additional parameters
ngrams_dict <- ngrams_dictionary(texts, clean = TRUE, ngram_min = 1, ngram_max = 5, points_filter = 1)
custom_segmented <- ngrams_segmentation(
  "thisisscience",
  ngrams_dict,
  simplify = TRUE,
  omit_zero = TRUE,
  score_formula = "points / words.number ^ 2"
)
print(custom_segmented)
```

## License

NUSS is licensed under the GPL-3.0 License.

## Contact

For any questions or issues, please open an issue on GitHub or contact the maintainer.

I hope you find NUSS useful for your text processing tasks!
