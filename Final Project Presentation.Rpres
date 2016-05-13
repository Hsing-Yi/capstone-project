Next Word Prediction
========================================================
author: Hsing-Yi Lin
date: 2016/05/13
autosize: true

Objective
========================================================
Around the world, people are spending an increasing amount of time on their mobile devices for email, social networking, banking and a whole range of other activities. But typing on mobile devices can be a serious pain. 
The product is a predictive text models, When someone types sentence, the keyboard presents five options for what the next word might be.
The essence of this project is to take a corpus (a body) of text from various sources, clean and analyze that text data, and build a predictive model to present the next likely word based on the prior two words provided by a user.

**Capabilities includes:**
- Predict next word based on users' input
- Calculate the percentages by suggested words



Concepts and Methodology for Next Word Prediction
========================================================
**Data cleaning**
- Adopted data cleaning steps, including (1) Convert into ASCII format  (2) Remove punctuation (3) Remove numeric characters (4)  Removes all single letters except "a" and "i".

**Training Data Selection**
- Through lexiture diveation check result, decide to select 25% data as training set to capture enough informations into the model.

**Modeling methodology**
- Used natural language processing techniques and Markov Models, choosing 3-grams and backoff method to do model training. Adopted likelihood ratio to identify possible 5 predictions values.


Model Result
========================================================

**Model Accuracy**
Random selected 30 sentenses from News, Blogs and Twitter and get average 20% accuracy.
- News : test 30 sentenses, get 22% accuracy
- Blogs : test 30 sentenses, get 20% accuracy
- Twitters: test 30 sentenses, get 17% accuracy

**Product Screen**
The next word prediction application is published in shiny apps.
The system will auto predict next words shown in the right screen.
Once the prediction word shown, you can choose one from prediction as next input word. And the system will auto based on the input to predict next word.

![alt text](screen_capture.png)


Conclusion
========================================================
To summarize, the way to provide complex language behavior into the model can be improved in following categories.
- Morphology : The study of the meaningful components of words.
- Syntax : The study of the structural relationships between words.
- Semantics : The study of meaning.

The execise here is the most simple example only use the propabilities analysis follow the Markov matrix to do prediction.
- The link to shiny app : <https://hsing-yi.shinyapps.io/shinyAp/>
- The rPresentation slides in <http://rpubs.com/Hsing-Yi/NextWordPrediction>

