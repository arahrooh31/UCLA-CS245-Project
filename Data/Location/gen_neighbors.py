import json

states = []
state_ids = {}
with open('states_cleaned.json') as f:
    states = json.load(f)
for id, state in enumerate(states):
    state['Id'] = id
    state_ids[state['Code']] = id

lines = []
with open('county_adjacency_cleaned.txt', encoding='iso-8859-1') as f:
    lines = f.readlines()

counties = []
county_ids = {}
id = 0
for line in lines:
    fields = line.split('\t')
    if fields[0]:
        county = fields[0].strip('"')
        state = county[-2:]
        zip = fields[1]
        counties.append({'Name': county.encode().decode(), 'State': state_ids[state], 'Zip': zip, 'Neighbors': [], 'Id': id})
        county_ids[' '.join([county, zip])] = id
        id += 1

def county_id(county, zip):
    county_decoded = county.strip('"')
    return county_ids[' '.join([county_decoded, zip])]
current_county_id = 0
for line in lines:
    fields = line.split('\t')
    if fields[0]:
        current_county_id = county_id(fields[0], fields[1])
    adj_county_id = county_id(fields[2], fields[3].rstrip())
    if current_county_id != adj_county_id:
        counties[current_county_id]['Neighbors'].append(adj_county_id)

with open('county_adjacency.json', 'w', encoding='utf-8') as f:
    json.dump({'States': states, 'Counties': counties}, f, ensure_ascii=False)
