import pandas as pd
from sodapy import Socrata


client = Socrata("data.cdc.gov", None)


# First 100000 results, returned as JSON from API / converted to Python list of dictionaries by sodapy
results = client.get("n8mc-b4w4", limit=100000)

# Convert to pandas DataFrame
results_df = pd.DataFrame.from_records(results)

print(results_df.head)

results_df.to_csv("COVID-19 Case Surveillance Public Use Data with Geography.csv")

len(results_df)
print(results)

