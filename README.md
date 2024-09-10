# Forensically useful mid-term and short-term temperature reconstruction for quasi-indoor death scenes
#### authors: Jędrzej Wydra, Łukasz Smaga, Szymon Matuszewski

## Abstract
While estimating postmortem interval (PMI) ambient temperature plays pivotal role, so its reconstruction is crucial for forensic scientists. The recommended procedure is to correct temperatures from the nearest meteorological station based on measurements from the death scene; typically applying linear regression. Recently, there were attempts to use different algorithms, which can improve that correction, for example GAM algorithm. Unfortunately, the improvements are usually a consequence of using more dependent variables than just the temperature from the death scene (e.g. humidity), which is impractical.

This study develops a practical new methods to accurately reconstruct ambient temperatures at a death scene, using just temperature measurements. Since the main difficulty preventing practitioners from using the correction protocol more frequently is likely the need to record temperatures on site for at least several days, we searched for the possibilities to shorten the measurement period. For this purpose we tested two less popular algorithms, which gave hope for this shortening. They were the concurrent regression model (the model from the functional data analysis field) for the mid-term reconstruction (measurements lasting several days) and the functional model based on Fourier expansion for the short-term reconstruction (measurements lasting a few hours).

Performance of the algorithms was tested using data collected in six places: roof and attic of the heated building, unheated garage inside the heated building, unheated wooden shack, uninhabited building and underground (the data logger was buried about 30 cm below the ground level). We classified these places as quasi-indoor conditions in contrast to typical indoor conditions, where temperatures are nearly constant and typical outdoor conditions in case of which there is no heat insulation.

The mid-term model reduced error compared to the linear regression, providing nearly perfect reconstruction for measurement periods longer than six days. More importantly, however, the accuracy of short-term reconstruction was also high. The short-term model closely matched the concurrent regression model’s performance after only four to five hours of measurements.

In practice, both methods are very similar to the standard procedure. The main difference is the change in algorithm and its implementation. In conclusion, this study demonstrates that correction of station temperatures can provide fairly accurate temperature data for use in estimating PMI after only 4-5 hours of measurements on a death scene.

## History
This project was a rollercoaster ride of epic proportions, filled with both exhilarating highs and nerve-wracking lows. It was an ambitious endeavor from the start, and there were more than a few moments when I seriously feared it might end up in the dreaded “cancelled projects” folder. But, as they say, fortune favors the bold, and after navigating through a series of near disasters, we pulled it off. In the end, I consider this to be one of my greatest successes—proof that even when things look bleak, perseverance can lead to something truly remarkable.

## Disclaimer
The article for this project is not yet published, and I don’t feel that the manuscript is quite ready for a pre-print just yet. However, in the near future, this repository will include the pre-print. R scripts and data are now avaliable in R-Laboratory. Python scripts and a GUI program (for non-technical users who are interested in exploring the results) are avalialbe in Python_software_beta_GUI. Python is needed to run our software. Executable version will be avaliable in future.
Only beta versions will be avaliable here, final version of software will be published as another project.
