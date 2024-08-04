import requests
import csv
import time

def find_pharmacies_in_area(api_key, location, radius):
    # Define the endpoint and parameters for the Google Places API
    endpoint_url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
    params = {
        'location': location,
        'radius': radius,
        'type': 'pharmacy',
        'key': api_key
    }
    
    pharmacies = []
    while True:
        response = requests.get(endpoint_url, params=params)
        results = response.json().get('results', [])
        
        for place in results:
            pharmacy = {
                'name': place.get('name'),
                'latitude': place['geometry']['location']['lat'],
                'longitude': place['geometry']['location']['lng']
            }
            pharmacies.append(pharmacy)
        
        next_page_token = response.json().get('next_page_token')
        if next_page_token:
            params['pagetoken'] = next_page_token
            time.sleep(2)  # Short delay before using the next_page_token
        else:
            break
    
    return pharmacies

def find_pharmacies_in_sydney(api_key):
    sydney_center = (-33.899890,150.980259)  # Latitude and Longitude of a rough Sydney centre
    radius = 2500  # 2.5 km radius for each grid cell to check for pharamacies - will undercount if more than 60 within radius
    
    # Define the search grid (latitude and longitude deltas)
    lat_delta = 0.015
    lng_delta = 0.015
    
    pharmacies = []
    for lat in range(-30, 30):
        for lng in range(-30, 30):
            location = f"{sydney_center[0] + lat * lat_delta},{sydney_center[1] + lng * lng_delta}"
            print(f"Searching in area centered at {location}")
            area_pharmacies = find_pharmacies_in_area(api_key, location, radius)
            pharmacies.extend(area_pharmacies)
            
            # Remove duplicates based on latitude and longitude
            pharmacies = [dict(t) for t in {tuple(d.items()) for d in pharmacies}]
            
            # Stop if we've reached the desired number of results
            if len(pharmacies) >= 2000:
                pharmacies = pharmacies[:2000]
                return pharmacies
    
    return pharmacies

def save_to_csv(pharmacies, filename):
    headers = ['Name', 'Latitude', 'Longitude']
    with open(filename, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=headers)
        writer.writeheader()
        for pharmacy in pharmacies:
            row = {
                'Name': pharmacy['name'],
                'Latitude': pharmacy['latitude'],
                'Longitude': pharmacy['longitude']
            }
            writer.writerow(row)

# Replace with your actual Google Places API key
api_key = 'GOOGLE_MAPS_API_KEY'
pharmacies = find_pharmacies_in_sydney(api_key)

# Save the results to a CSV file
csv_filename = 'pharmacies_in_sydney_grid_finer.csv'
save_to_csv(pharmacies, csv_filename)

print(f"Saved {len(pharmacies)} pharmacies to {csv_filename}")