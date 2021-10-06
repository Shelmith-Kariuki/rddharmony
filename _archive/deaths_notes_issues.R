#Notes:
# "Both sexes"
#"752 - Sweden - VR - Deaths - 2015 - Register - Eurostat Database - Year of occurrence - Direct - High quality" is a case where we have complete series only
# "752 - Sweden - VR - Deaths - 2006 - Register - Eurostat Database - Year of occurrence - Direct - High quality" is a case where we have complete series only
# "752 - Sweden - VR - Deaths - 1950 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair" is abridged only
# "752 - Sweden - VR - Deaths - 1961 - Register - WHO-IGME Mortality Data base - Unknown - Direct - Fair" is a case where we do not have Both sexes and we do not have indicator 188 data
# "752 - Sweden - VR - Deaths - 2012 - Register - WHO-IGME Mortality Data base - Unknown - Direct - Fair" is a case that does not have both sexes, 188 data, only abridged and also has 1,2,3,4 instead of 1-4 in the data
# "116 - Cambodia - Census - Deaths - 2007-2008 - Census - Tabulations provided by IPUMS International Database - De-facto - Household deaths - Fair" is a perfect example of a case where the inicator 188 total matches with indicator 194 total
# "533 - Aruba - VR - Deaths - 1987 - Register - WHO Mortality Data base - Unknown - Direct - High quality" has the 1,2,3,4 issue. But 194 total == 188 total.
# "533 - Aruba - VR - Deaths - 2020 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair" has both abridged and complete series
# "533 - Aruba - VR - Deaths - 2012 - Register - WHO Mortality Data base - Unknown - Direct - High quality" also has the 1,2,3,4 problem
# "533 - Aruba - VR - Deaths - 2005 - Register - WHO Mortality Data base - Unknown - Direct - High quality" has the 1,2,3,4 issue but total 188 tallys with total 194
# "380 - Italy - VR - Deaths - 1959 - Register - WHO Mortality Data base - Unknown - Direct - High quality has the 1,2,3,4"
# "380 - Italy - VR - Deaths - 1965 - Register - Eurostat Database - Year of occurrence - Direct - High quality" only has Total label
# "784 - United Arab Emirates - VR - Deaths - 2006 - Register - Births and Deaths 2006 - Year of occurrence - Direct - Fair" one of the perfect ones
# "702 - Singapore - VR - Deaths - 2001 - Register - WHO Mortality Data base - Unknown - Direct - High quality" looks perfect, total_188 == total_194_195 though it has 1,2,3,4 issues.
# "702 - Singapore - VR - Deaths - 2010 - Register - Demographic Yearbook - Year of registration - Direct - Fair" has an issue where the data has both abridged and complete series
# but the complete only has the minimum age, the closing age, total and/or unknown.
# **"702 - Singapore - VR - Deaths - 2010 - Register - Demographic Yearbook - Year of registration - Direct - Fair" indicator 188 shows that we need to check for a total that matches indicator 188 before we check
# for the latest data source year, since the latest total value could be very wrong.
# "702 - Singapore - VR - Deaths - 1969 - Register - WHO Mortality Data base - Unknown - Direct - High quality" has 1,2,3,4 issue
# "702 - Singapore - VR - Deaths - 1996 - Register - Demographic Yearbook - Year of registration - Direct - Fair" looks really really perfect. Hoping that the totals tally.
# "624 - Guinea-Bissau - VR - Deaths - 1951 - Register - Demographic Yearbook - Year of occurrence - Direct - Low" only has 188 data, so it should be dropped and not appear in the final data

# "Female"
# "624 - Guinea-Bissau - VR - Deaths - 1969 - Register - Demographic Yearbook - Year of occurrence - Direct - Low" has very wide age groups and also has
## both abridged and complete series where the complete only has the minimum age, the closing age, total and/or unknown.
# "624 - Guinea-Bissau - VR - Deaths - 1970 - Register - Demographic Yearbook - Year of occurrence - Direct - Low" has the same issue as above
# "659 - Saint Kitts and Nevis - VR - Deaths - 1998 - Register - Demographic Yearbook - Year of registration - Direct - Fair" has same issue as above
# "64 - Bhutan - Census - Deaths - 2005 - Census - Demographic Yearbook - De-facto - Household deaths - Unknown" has the issue above
# "64 - Bhutan - Census - Deaths - 2005 - Census - Demographic Yearbook - De-facto - Household deaths - Unknown"
# "624 - Guinea-Bissau - Census - Deaths - 2008-2009 - Census - Guinea-Bissau 2009 Census - De-facto - Household deaths - Fair" is amazing ... totals match the sum of data values
# "426 - Lesotho - VR - Deaths - 2017 - Register - Demographic Yearbook - Year of registration - Direct - Low" has 0-4 instead of <1 and 1-4. Both series but complete only has Total and oag
# "426 - Lesotho - Census - Deaths - 2006 - Census - 2006 Lesotho Population and Housing Census: Volume II Census Tables - De-jure - Household deaths - Fair" doesn't have Total
# "426 - Lesotho - Census - Deaths - 1996 - Census - Personal communication - De-jure - Household deaths - Fair" has the issue as above.
# "426 - Lesotho - Census - Deaths - 1985-1986 - Census - 1986 Population Census Statistical Tables: Vol. 2 - De-jure - Household deaths - Fair" is perfect
# "659 - Saint Kitts and Nevis - VR - Deaths - 2007 - Register - WHO-IGME Mortality Data base - Unknown - Direct - Fair" has the 1,2,3,4 issue
# "659 - Saint Kitts and Nevis - VR - Deaths - 1990 - Register - WHO Mortality Data base - Unknown - Direct - High quality" has the 1,2,3,4 issue
# "388 - Jamaica - VR - Deaths - 1982 - Register - WHO Mortality Data base - Unknown - Direct - High quality" has wide age groups
# "388 - Jamaica - VR - Deaths - 1961 - Register - WHO Mortality Data base - Unknown - Direct - High quality" has the issue as above
# **"388 - Jamaica - Census - Deaths - 2011 - Census - Demographic Yearbook - De-facto - Household deaths - Unknown" has three open age groups
# "478 - Mauritania - Census - Deaths - 1988 - Census - Mauritanie. Résultats prioritaires du recensement de la population et de l'habitat 1988 - Volume I - De-jure - Household deaths - Fair" doesn't have totals recorded
# "324 - Guinea - Census - Deaths - 2014 - Census - Demographic Yearbook - De-facto - Household deaths - Unknown" has the issue where 188 total latest value is not the correct one.
# "324 - Guinea - Census - Deaths - 2014 - Census - Demographic Yearbook - De-facto - Household deaths - Unknown" has both abridged and complete series that are kinda full
# "324 - Guinea - Census - Deaths - 1996 - Census - Demographic Yearbook - De-facto - Household deaths - Unknown" has both abridged and complete series that are kinda full.
# "348 - Hungary - VR - Deaths - 2010 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair" is a complete series only
# "710 - South Africa - VR - Deaths - 2000 - Register - WHO Mortality Data base - Unknown - Direct - High quality"
# "776 - Tonga - VR - Deaths - 1996 - Register - Demographic Yearbook - Year of registration - Direct - Fair" has wide age groups
#
# After Part 1
# sorted: "694 - Sierra Leone - VR - Deaths - 2010 - Register - Demographic Yearbook - Year of registration - Direct - Low" wide age groups are being dropped.
#
#
#
# After Part 2:
# The complete series should have been dropped: "694 - Sierra Leone - Census - Deaths - 2004 - Census - Demographic Yearbook - De-facto - Household deaths - Unknown"
# Same case with "694 - Sierra Leone - Census - Deaths - 2014-2015 - Census - UN-IGME Country Consultation - Unknown - Household deaths - Fair"
#
