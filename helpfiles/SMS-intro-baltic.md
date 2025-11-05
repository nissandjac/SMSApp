---
title: 'SMS prognoser'
output:
  html_document:
    keep_md: true
---

This section provides some background information for this App. To make forecasts, you have to push **"Simple predictions"** or **"Detailed predictions"** in the top of the screen. The calculations for the Simple predictions are the same as for the Detailed predictions, but simpler output are produced for the simple prediction, so this model type runs faster.


### Background
This App makes forecast scenarios for the main fish stocks in the Baltic Sea, based on the results from the Stochastic Multispecies Model (SMS) used by ICES to provide multispecies mortalities. Future fishing pressure and exploitation pattern can be changed for the stocks and the model will then calculate future fishing yield and stock sizes. SMS takes into account that fish eats fish, so a change in fishing pressure for a given species will directly change its stock size and yield, but may also change stock size and yield for other species which are prey or predator for the given species.   


<table width="70%">
<tr><td colspan=4><b>The model has 3 stocks:</td></tr>
<tr><td colspan=4><hr noshade></td></tr>
<tr> <td align=left><b></b></td> <td align=right><b>Danish stock name</b></td>	<td align=right><b>Stock name</b></td>	<td align=right><b>Type</b></td> </tr>
<tr> <td align=left><b>1</b></td><td align=right>Torsk</td>	<td align=right>Cod</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>2</b></td><td align=right>Sild</td>	<td align=right>Herring</td>	<td align=right>Dynamic prey</td></tr>
<tr> <td align=left><b>3</b></td><td align=right>Brisling</td>	<td align=right>Sprat</td>	<td align=right>Dynamic prey</td></tr>
<tr><td colspan=4><hr noshade></td></tr>
</table><br>
<br>

There are two main types of stocks shown in the table: "Other predator" and "Dynamic stocks". The group of **Other predators** includes cod. It is assumed that the we know the abundance of cod in the model, which will be kept constant if the user does not change it. The group of **Dynamic stocks** includes the main commercial fish stocks, where stock sizes change dynamically from recruitment, fishing pressure and natural mortality. 

The SMS model is used by the ICES Working Group on Multispecies Assessment Method, WGSAM (ICES, 2022) to estimate the historical natural mortalities, which are used in the ICES stock assessment and TAC advice for number of species in the Baltic Sea area. The model is a so-called multispecies model, which uses historical catches, catch rates from scientific surveys and the observed stomach content from a 65,000 cod to estimate the historical fishing mortality (F) and stock sizes. The model takes account for predation, which results in the so-called predation mortality (M2). 

SMS is most often applied to estimate the historical stock sizes and fishing mortalities. The results are close to the results from the ICES single stock assessments, but not identical as the ICES models for the individual species are often more complex than SMS. 

In this App, SMS is used as a forecast model. This is done from the model parameters, e.g. food suitability and exploitation pattern, estimated in the historical SMS, and assumptions of future fishing pressure and recruitment. The forecasts or scenarios assumes that everything is kept constant in the future, if not changed by the user. The is a crude assumption when the forecast is made for a long time period. This and the fact that there in general is rather high uncertainties in a complex model like SMS, means that the results should be seen a model results rather than strict prediction of future changes.


