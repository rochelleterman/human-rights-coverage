human-rights-coverage
=====================

Takes lexisnexis articles on human rights and analyzes them.

Rochelle Terman

University of California, Berkeley

rterman@gmail.com

Summary
-------

This project:

1. Takes a bunch of articles downloaded form Lexis Nexis (see below in "Before Starting")
2. Cleans and categorizes these articles; adds country, region, and subject metadata.
3. Does descriptive analysis, such as plotting number of articles by region over time.
4. Performs automated text analysis on the corpora of articles, including word separating algorithms.

Before Starting
---------------

If you want to use this on your own data, you must first collect the data in the proper format. To do this:

1. Download a bunch of articles in Lexis Nexis
2. Parse them into a CSV. I used [Neal Caren's python script](http://nealcaren.web.unc.edu/cleaning-up-lexisnexis-files/) to do this.
3. Put the data in the /Data directory.

Order of Scripts
----------------

Run the scripts in the following order:

1. load_and_clean.R : Loads the CSV files in /Data and concatenates them.
2. countries_and_regions.R: Adds country and region metadata to articles.
3. subject.percentages.R : Adds subject metadata to articles and analyzes them. Writes subject spreadsheets to Results.
4. geo_aggregation.R : Performs descriptive analysis of articles by geography, including number of articles per country & region over time. Writes a number of spreadsheets to Results.
5. corpora_writing.R : Writes corpus of articles to prepare for automated text analysis.
6. NYT_corpus.ipynb : Preprocesses corpus (made above) in preparation for text analysis - i.e. writes DTM.
7. distinctive_words.R : Applies 3 word separating algorithsms to corpus. Writes scores to Results.

