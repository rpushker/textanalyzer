---
title: 'TextAnalyzeR: an R Package to Analyze Text'
author: "Ravindra Pushker"
output:
  html_document:
    keep_md: TRUE
---

# Analyze text 

## Load library


``` r
library(textanalyzer)
library(knitr)
```

## Get sample text


``` r
in_text <- c("The wealth of genomic data in bacteria is helping microbiologists understand the factors involved in gene innovation. Among these, the expansion and reduction of gene families appears to have a fundamental role in this, but the factors influencing gene family size are unclear. The relative content of paralogous genes in bacterial genomes increases with genome size, largely due to the expansion of gene family size in large genomes. Bacteria undergoing genome reduction display a parallel process of redundancy elimination, by which gene families are reduced to one or a few members. Gene family size is also influenced by sequence divergence and physiological function. Large gene families show wider sequence divergence, suggesting they are probably older, and certain functions (such as metabolite transport mechanisms) are overrepresented in large families. The size of a given gene family is remarkably similar in strains of the same species and in closely related species, suggesting that homologous gene families are vertically transmitted and depend little on horizontal gene transfer (HGT). The remarkable preservation of copy numbers in widely different ecotypes indicates a functional role for the different copies rather than simply a back-up role. When different genera are compared, the increase in phylogenetic distance and/or ecological specialization disrupts this preservation, albeit in a gradual manner and maintaining an overall similarity, which also supports this view. HGT can have an important role, however, in nonhomologous gene families, as exemplified by a comparison between saprophytic and enterohemorrhagic strains of Escherichia coli.", 
"Pseudogenes are nonfunctional DNA sequences that can accumulate in the genomes of some bacterial species, especially those undergoing processes like niche change, host specialization, or weak selection strength. They may last for long evolutionary periods, opening the question of how the genome prevents expression of these degenerated or disrupted genes that would presumably give rise to malfunctioning proteins. We have investigated ribosomal binding strength at Shine-Dalgarno sequences and the prevalence of sigma70 promoter regions in pseudogenes across bacteria. It is reported that the RNA polymerase-binding sites and more strongly the ribosome-binding regions of pseudogenes are highly degraded, suggesting that transcription and translation are impaired in nonfunctional open reading frames. This would reduce the metabolic investment on faulty proteins because although pseudogenes can persist for long time periods, they would be effectively silenced. It is unclear whether mutation accumulation on regulatory regions is neutral or whether it is accelerated by selection.",
"Current human activities undoubtedly impact natural ecosystems. However, the influence of Homo sapiens on living organisms must have also occurred in the past. Certain genomic characteristics of prokaryotes can be used to study the impact of ancient human activities on microorganisms. By analyzing DNA sequence similarity features of transposable elements, dramatic genomic changes have been identified in bacteria that are associated with large and stable human communities, agriculture and animal domestication: three features unequivocally linked to the Neolithic revolution. It is hypothesized that bacteria specialized in human-associated niches underwent an intense transformation after the social and demographic changes that took place with the first Neolithic settlements. These genomic changes are absent in related species that are not specialized in humans."
)
```

## Get tokens

``` r
tokens <- analyze_ngrams(in_text, n=1)
```


``` r
head(tokens)
```


|word     |  n|
|:--------|--:|
|gene     | 11|
|families |  6|
|bacteria |  5|
|size     |  5|
|family   |  4|
|genomic  |  4|


``` r
plot_ngrams(tokens, plot_nrows = 10)
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## Get bigrams

``` r
bigrams <- analyze_ngrams(in_text, n=2, top_rows = 25)
```


``` r
head(bigrams)
```


|bigram              |  n|word                |
|:-------------------|--:|:-------------------|
|gene families       |  5|gene families       |
|gene family         |  4|gene family         |
|family size         |  3|family size         |
|human activities    |  2|human activities    |
|related species     |  2|related species     |
|sequence divergence |  2|sequence divergence |


``` r
plot_ngrams(bigrams, plot_nrows = 10)
```

![](README_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

# Get trigrams

``` r
trigrams <- analyze_ngrams(in_text, n=3)
```


``` r
head(trigrams)
```


|trigram                       |  n|word                          |
|:-----------------------------|--:|:-----------------------------|
|gene family size              |  3|gene family size              |
|activities undoubtedly impact |  1|activities undoubtedly impact |
|analyzing dna sequence        |  1|analyzing dna sequence        |
|ancient human activities      |  1|ancient human activities      |
|bacteria undergoing genome    |  1|bacteria undergoing genome    |
|bacterial genomes increases   |  1|bacterial genomes increases   |


``` r
plot_ngrams(trigrams, plot_nrows = 10)
```

![](README_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
