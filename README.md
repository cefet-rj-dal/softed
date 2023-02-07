# <img src="softed_logo.png" width="12%" /> : Metrics for Soft Evaluation of Time Series Event Detection

SoftED metrics are a new set of metrics designed for soft evaluating event detection methods. They enable the evaluation of both detection accuracy and the degree to which their detections represent events. They improved event detection evaluation by associating events and their representative detections, incorporating temporal tolerance in event detcetion compared to the usual classification metrics. SoftED metrics contribute to detection evaluation and method selection.

This repository presents the implementation of SoftED metrics, as well as the datasets and experimental evaluation codes and results.

Folders are organized as follows:
* __softed_metrics:__ implementation of SoftED metrics in R
* __detection_data:__ Datasets adopted in experimental evaluation
* __experiment_code:__ Experimental evaluation codes
* __metrics_results:__ Results of detection evaluations based on the adopted metrics
* __presentations:__ Presentations regarding SoftED metrics
* __quali_survey:__ Qualitative analysis and survey results

This readme gives a brief overview and contains examples of usage of the SoftED metrics for evaluating a particular time series event detection method. Please refer to the following for more details about SoftED formalization and experimental evaluation:
* __Published Paper__:
* __Experimental evaluation - Wiki page: 

## SoftED R implementation

The implementation of SoftED metrics are available in R and can be directly downloaded from [softed_metrics.r](https://github.com/cefet-rj-dal/softed/blob/main/softed_metrics/softed_metrics.r).

The use of the metrics are independent from the adopted detection method. Based on the detection results of a given times series event detection method, a simple example of usage is given by:
``` r
soft_evaluate(events, reference, k=15)
```
__Input:__
* _events_: A data.frame with at least one variables: time (events time/indexes)
* _reference_: data.frame of the same length as the time series with two variables: time, event (boolean indicating true events)

__Output:__
* calculated metrics values.

#### Example:  soft evaluation of a detection method

``` r
library(EventDetectR)

# === GECCO Dataset ===
serie <- geccoIC2018Train[16500:18000,]
serie <- subset(serie, select=c(Time, Trueb))
reference <- subset(train, select=c(Time, EVENT))
names(reference) <- c("time","event")
```

``` r
source("https://raw.githubusercontent.com/cefet-rj-dal/softed/experiment_code/harbinger.R")
source()
```
``` r
#Detect
events <- evtdet.seminalChangePoint(serie, w=50,na.action=na.omit) #SCP
#Plot
print(evtplot(serie,events, reference))
```
``` r
#Hard evaluate
evaluate(events, reference)

#Evaluate
soft_evaluate(events, reference, k=15)
```

### SoftED in the Harbinger package

The [Harbinger R-package](https://github.com/cefet-rj-dal/harbinger) implements the [Harbinger framework](https://eic.cefet-rj.br/~dal/harbinger/) designed for integration and analysis of event detection methods. 
Recently, it has been extended with the addition of the SoftED metrics for allowing soft evaluation of the detection performance of times series event detection methods.

Jupyter Notebook with an example of usage: https://nbviewer.org/github/cefet-rj-dal/harbinger/blob/master/examples/har_softed.ipynb

## SoftED experimental evaluation

SoftED metrics were submitted to an experimental evaluation to analyze their contribution against the traditional classification metrics (hard metrics) and the [NAB score](https://doi.org/10.1109/ICMLA.2015.141)([Github](https://github.com/numenta/NAB)), both being the current state-of-the-art in detection scoring.

The experimental settings, datasets and codes of the experimental evaluation of SoftED metrics are described in detail in the wiki page: 
