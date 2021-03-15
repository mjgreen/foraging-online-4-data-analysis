---
title: "foraging-online-4"
output: 
  html_document:
    toc: true
    keep_md: TRUE
---
<style type="text/css">
.main-container {
  max-width: 1800px;
  margin-left: auto;
  margin-right: auto;
}
</style>





## Part 1 read in the data

From previously-generated csv files.




## Part 2 explore the data

Just plot all 4 metrics.

![](part2_explore_the_data_files/figure-html/Just plot all 4 metrics-1.png)<!-- -->

How score varies

![](part2_explore_the_data_files/figure-html/How score varies-1.png)<!-- -->



Bananas are very rarely ignored. Apples are ignored more often, and they get ignored more as the trials progress.

![](part2_explore_the_data_files/figure-html/Rejection rate in each condition for each fruit type, by trial-1.png)<!-- -->

Rejection rate for apples for each subject, by trial

![](part2_explore_the_data_files/figure-html/Rejection rate for apples for each subject, by trial-1.png)<!-- -->

Same but with free y axis

![](part2_explore_the_data_files/figure-html/Rejection rate for apples for each subject, by trial, free y-1.png)<!-- -->

Rejection rate against score

![](part2_explore_the_data_files/figure-html/Rejection rate against score-1.png)<!-- -->



<!-- In the rare bananas condition, ignoring apples is associated with lower scores. In the frequent bananas condition, ignoring apples is associated with higher scores. -->

<!-- ```{r, fig.width=7,fig.height=4} -->
<!-- d=read_csv("d3_trial_by_trial.csv") -->
<!-- ggplot(d, aes(y=ignap, x=score))+ -->
<!--   facet_wrap(~condition)+ -->
<!--   geom_jitter()+ -->
<!--   geom_smooth(aes(colour=condition),method='lm',formula='y~x')+theme(aspect.ratio =1) -->
<!-- ``` -->

<!-- Larger inter-click durations (slower moving around) is associated with lower scores. -->

<!-- ```{r, fig.width=7,fig.height=4} -->
<!-- d=read_csv("d3_trial_by_trial.csv") -->
<!-- ggplot(d, aes(x=score, y=icint))+ -->
<!--   facet_wrap(~condition)+ -->
<!--   geom_jitter()+ -->
<!--   geom_smooth(aes(colour=condition),method='lm',formula='y~x')+theme(aspect.ratio =1) -->
<!-- ``` -->

<!-- foo -->

<!-- ```{r, fig.width=7,fig.height=4} -->
<!-- d=read_csv("d3_trial_by_trial.csv") -->
<!-- ggplot(d, aes(x=ignap, y=ignba))+ -->
<!--   facet_wrap(~condition)+ -->
<!--   geom_jitter()+ -->
<!--   geom_smooth(aes(colour=condition),method='lm',formula='y~x')+theme(aspect.ratio =1) -->
<!-- ``` -->

