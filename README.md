# RShiny-Dashboard
Interactive dashboard that helps visualize how different GICS sectors of S&amp;P 500 companies perform in terms of Planet, People and Governance metrics outlined by the World Economic Forum.

The dynamic RShiny dashboard includes both the user interface code and the back-end server code. 
(A) The **user interface** includes 
1. a sidebar that allows the selection of a GICS sector or all sectors altogether to dictate visualizations.
2. Three tabs that allow the selection of
   i.) dynamic visualizations in the 'plot' tab
   ii.) The context Reference of the data used and plug-ins to the Centre for Ipact's and Open for Good Initiative's webpages.
   iii.) Methodology followed to create visualizations and any disclaimers for data manipulation/transformation.
3. Pixel resolution and arrangement of plots by dictating the rows and columns on the webpage.

(B) The back-end **server** code includes
Dynamic switching of visualizations based on user selection and backend calculation of sample sizes deduced from user selection. Alternate calculation routes for the 'All' sectors selection and a single sector selection.
The visualizations use UCLA branding colors and font sizes and colors to improve accessibility.
  i.) Average Disclosure Rate by Pillar (Planet, People or Governance)
  ii.) Audit status of reported metrics (Full, partial or no disclosure)
  iii.) Greenhouse Gas Emissions (GHG) Disclosure rate for each of Scope 1, Scope 2 and Scope 3 GHG - Planet/ Environmental Pillar
  iv.) Average Employee Ethnic Diversity Disclosure per sector - People/ Social Pillar
  v.) Quantified Scope 1,2 and 3 GHG emissions (in metric tons) per sector along with the breakdown of the number of companies that report each of the three scopes - Planet/ Environmental Pillar
  vi.) Average percentage of employees belonging to different ethnic groups based on the Equal Employment Opportunity Form (EEO1): Asian, Hispanic, Black and White - People/ Social Pillar
  vii.) Average percentage of women employees across different GICS sectors - People/ Social Pillar
  viii.) CEO to Median Employee Compensation Ratio to understand equity in the sector. The values here represent the ratio of the CEO's total compensation to the median employee's compensation.
        For example, a value of 1000 indicates that the CEO's compensation is 1000 times more than their median employee's. - Governance Pillar
   
  

