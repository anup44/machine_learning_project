from flask import Flask, request, jsonify, Response
import json
import re
import selectorlib
import requests
from dateutil import parser as dateparser
app = Flask(__name__)
extractor = selectorlib.Extractor.from_yaml_file('selectors.yml')

def scrape(url):    
    headers = {
        'authority': 'www.amazon.com',
        'pragma': 'no-cache',
        'cache-control': 'no-cache',
        'dnt': '1',
        'upgrade-insecure-requests': '1',
        'user-agent': 'Mozilla/5.0 (X11; CrOS x86_64 8172.45.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.64 Safari/537.36',
        'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
        'sec-fetch-site': 'none',
        'sec-fetch-mode': 'navigate',
        'sec-fetch-dest': 'document',
        'accept-language': 'en-GB,en-US;q=0.9,en;q=0.8',
    }

    # Download the page using requests
    print("Downloading %s"%url)
    r = requests.get(url, headers=headers)
    # Simple check to check if page was blocked (Usually 503)
    if r.status_code > 500:
        if "To discuss automated access to Amazon data please contact" in r.text:
            print("Page %s was blocked by Amazon. Please try using better proxies\n"%url)
        else:
            print("Page %s must have been blocked by Amazon as the status code was %d"%(url,r.status_code))
        return None
    # Pass the HTML of the page and create 
    data = extractor.extract(r.text,base_url=url)
    if sum([data[k] is not None for k in data]) <= 0:
        return None
    reviews = []
    if data['reviews'] is None:
        data['reviews'] = []
    for r in data['reviews']:
        if r['title'] is None:
            continue
        r["product"] = data["product_title"]
        r['url'] = url
        if 'verified_purchase' in r:
            if r['verified_purchase'] and 'Verified Purchase' in r['verified_purchase']:
                r['verified_purchase'] = True
            else:
                r['verified_purchase'] = False
        r['rating'] = r['rating'].split(' out of')[0]
        date_posted = r['date'].split('on ')[-1]
        if r['images']:
            r['images'] = "\n".join(r['images'])
        r['date'] = dateparser.parse(date_posted).strftime('%d %b %Y')
        reviews.append(r)
    histogram = {}
    for h in data['histogram']:
        histogram[h['key']] = h['value']
    data['histogram'] = histogram
    data['average_rating'] = float(data['average_rating'].split(' out')[0])
    data['reviews'] = reviews
    # data['number_of_reviews'] = int(data['number_of_reviews'].split(' global ratings')[0].replace(',', ''))
    data['number_of_reviews'] = int(re.split(' global ratings?', data['number_of_reviews'])[0].replace(',', ''))
    return data 
    
@app.route('/')
def api():
    url = request.args.get('url',None)
    if url:
        data = scrape(url)
        # return jsonify(data, skipkeys=True)
        return Response(json.dumps(data), status=200, mimetype='application/json')
    return jsonify({'error':'URL to scrape is not provided'}),400

@app.route('/multi_page')
def api_multi_page():
    url = request.args.get('url', None)
    pages = int(request.args.get('pages', None))
    page_num = 1
    if url:
        data_out = scrape(url)
    else:
        return jsonify({'error':'URL to scrape is not provided'}),400
    if data_out is None:
        return jsonify(None)
    next_page = data_out['next_page']
    while(page_num < pages and next_page):
        if next_page:
            data = scrape(next_page)
            if data is None:
                return data_out
            data_out['reviews'] = data_out['reviews'] + data['reviews']
            data_out['next_page'] = data['next_page']
            page_num += 1
            next_page = data['next_page']
            
        else:
            data_out['pages_retreived'] = page_num
            return Response(json.dumps(data_out), status=200, mimetype='application/json')
    data_out['pages_retreived'] = page_num
    return Response(json.dumps(data_out), status=200, mimetype='application/json')
