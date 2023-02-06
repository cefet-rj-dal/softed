# SoftED: Metrics for Soft Evaluation of Time Series Event Detection

SoftED metrics are a new set of metrics designed for soft evaluating event detection methods. They enable the evaluation of both detection accuracy and the degree to which their detections represent events. They improved event detection evaluation by associating events and their representative detections, incorporating temporal tolerance in event detcetion compared to the usual classification metrics. SoftED metrics contribute to detection evaluation and method selection.

This repository presents the implementation of SoftED metrics, as well as the datasets and experimental evaluation codes and results.

Folders are organized as follows:
* __softed_metrics:__ implementation of SoftED metrics in R
* __detection_data:__ Datasets adopted in experimental evaluation
* __experiment_code:__ Experimental evaluation codes
* __metrics_results:__ Results of detection evaluations based on the adopted metrics
* __presentations:__ Presentations regarding SoftED metrics
* __quali_survey:__ Qualitative analysis and survey results

## SoftED R implementation

The implementation of SOftED metrics are available in R and can be directly downloaded from [softed_metrics.r](https://github.com/cefet-rj-dal/softed/blob/main/softed_metrics/softed_metrics.r).

The use of the metrics are independent from the adopted detection method. Based on the detcetion results of a given times series event detection method, a simple example of usage is given by:
``` r
soft_evaluate(events, reference, k=15)
```
__Input:__
* events: A data.frame with at least one variables: time (events time/indexes)
* reference: data.frame of the same length as the time series with two variables: time, event (boolean indicating true events)

__Output:__
* calculated metrics values.

## SoftED in the Harbinger package

The [Harbinger R-package](https://github.com/cefet-rj-dal/harbinger) implements the [Harbinger framework](https://eic.cefet-rj.br/~dal/harbinger/) designed for integration and analysis of event detection methods. 
Recently, it has been extended with the addition of the SoftED metrics for allowing soft evaluation of the detection performance of times series event detection methods.

Jupyter Notebook with an example of usage: https://nbviewer.org/github/cefet-rj-dal/harbinger/blob/master/examples/har_softed.ipynb
