SELECT * 
FROM forex.rates 
WHERE base_cur IN ({observed_currencies})
ORDER BY base_cur, conversion_cur, date