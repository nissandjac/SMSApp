---
title: 'SMS prognoser'
output:
  html_document:
    keep_md: true
---


Her kan du læse lidt om baggrunden for denne App. Hvis du ønsker at lave prognoser, skal du trykke på enten **Simple predictions** eller **Detailed predictions** i toppen af skærmen. Beregningerne for de simple prognoser er lige så komplekse som de detaljerede prognoser, men de simple prognoser giver kun oversigtsfigurer over den fremtidige udvikling og prognosebregningerne kan derfor gennemføres hurtigere. De detaljerede prognoser giver langt flere detaljer og tager derfor lidt længere tid at lave.

### Baggrund
Med denne App kan der laves prognoser for fiskebestandene i den østlige Østersø ud fra resultaterne fra ICES Stochastic Multispecies Model (SMS). Det fremtidige fiskeritryk og fiskerimønster kan ændres for en række bestande i Østersøen og modellen beregner derefter det fremtidige fiskeriudbytte og bestandenes størrelse. I beregningerne tages der hensyn til at fisk spiser fisk, så en ændring i fiskeritrykket for en art giver både ændringer i bestandsstørrelse og fiskeriudbytte for arten selv, men også andre arter der kan være byttedyr eller rovdyr for arten.



<table width="70%">
<tr><td colspan=4><b>Modellen har 3 bestande:</td></tr>
<tr><td colspan=4><hr noshade></td></tr>
<tr> <td align=left><b></b></td> <td align=right><b>Dansk navn</b></td>	<td align=right><b>Engelsk navn</b></td>	<td align=right><b>Type</b></td> </tr>
<tr> <td align=left><b>1</b></td><td align=right>Torsk</td>	<td align=right>Cod</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>2</b></td><td align=right>Sild</td>	<td align=right>Herring</td>	<td align=right>Dynamic prey</td></tr>
<tr> <td align=left><b>3</b></td><td align=right>Brisling</td>	<td align=right>Sprat</td>	<td align=right>Dynamic prey</td></tr>
<tr><td colspan=4><hr noshade></td></tr>
</table><br>


Der er er to hovedtyper af bestande vist i tabellen: "Andet rovdyr" og "Dynamiske bestande" , Gruppen af **Andet rovdyr** (”Other predators”) indeholder torsk, der alle spiser fisk. I modellen antages det at man kender antallet af torsk i prognosen og dette vil holdes konstant i modellen, hvis man da ikke ændre det. Gruppen af **Dynamiske bestande** indeholder vigtigste kommercielle bestande, hvor bestandstørrelsen udvikles dynamisk ud fra rekruttering, fiskeritryk og den naturlige dødelighed. 

SMS Modellen anvendes af ICES arbejdsgruppen WGSAM (ICES, 2022) til at bestemme de historiske naturlige dødeligheder, der anvendes i ICES bestandsvurderingerne og TAC-rådgivning for en række bestande i Nordsøen. Modellen er en såkaldt flerartsmodel, der ud de historiske fangster, fangstrater fra videnskabelige togter og observeret maveindhold fra en kvart million fisk, samt andre data, beregner de historiske Fiskeridødeligheder (F) og bestandsstørrelser. Modellen indregner at fisk spiser fisk, hvilket resulterer i den såkaldte predationsdødelighed (M2). 

SMS anvendes oftest til at bestemme hvad der er sket historisk, altså til at bestemme de historiske bestandsstørrelser og fiskeridødeligheder. Den historiske SMS giver stort set de samme resultater som i ICES bestandsvurderingen, men der er forskelle, der blandt andet skyldes at SMS er forskellig fra de modeller der oftest benyttes. 

I denne App anvendes SMS som prognosemodel, hvor der regnes frem i tiden. Dette gøres ud fra de modelparametre for fx fødevalg og fiskerimønstre, der er bestemt i den historiske SMS samt med antagelser om fremtidig fiskeritryk og rekruttering. Prognosen antager, at alt andet end det der ændres i modellen, fx fiskeritrykket, holdes konstant, hvilket er en meget grov antagelse specielt hvis der laves prognoser over mange år. Dette, sammen med en den usikkerhed der altid vil være i en så kompleks model som SMS, betyder at resultatet fra prognoserne skal mere ses som modelresultater end som realistiske forudsigelser for, hvad der vil ske over en længere årrække.
