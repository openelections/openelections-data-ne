from __future__ import print_function
from zipfile import ZipFile

try:
    from StringIO import StringIO as ioDuder
except ImportError:
    from io import BytesIO as ioDuder

import unicodecsv
import requests
import clarify


county = 'Sarpy'

url = 'http://results.enr.clarityelections.com/NE/Sarpy/54217/149572/en/summary.html'

election_type = 'general'

def clarify_sarpy():
    '''
    1. Fetch zipped XML file.
    2. Unzip in memory.
    3. Load into Clarify.
    4. Loop over results, write to file.
    '''

    # discover path to zipfile and fetch
    s = clarify.Jurisdiction(url=url, level='county')
    r = requests.get(s.report_url('xml'), stream=True)
    z = ZipFile(ioDuder(r.content))

    # hand off to clarify
    p = clarify.Parser()
    p.parse(z.open('detail.xml'))

    results = []
    
    # According to the Sarpy County election commissioner, results with a
    # `vote_type` of "underVotes" -- e.g., only 1 vote cast in a "pick 2"
    # race -- or "overVotes" -- e.g., 3 votes cast in a "pick 2" race --
    # are just context information and should not be included in results
    excluded_vote_types = ['underVotes', 'overVotes']

    for result in p.results:

        if result.vote_type not in excluded_vote_types:
            candidate = result.choice.text
            office, district = parse_office(result.contest.text)
            party = result.choice.party

            try:
                precinct = result.jurisdiction.name
            except AttributeError:
                precinct = None

            r = [x for x in results if x['precinct'] == precinct and \
                 x['office'] == office and x['district'] == district and \
                 x['party'] == party and x['candidate'] == candidate]

            if r:
                r[0][result.vote_type] = result.votes
            else:
                record = {
                    'county': county,
                    'precinct': precinct,
                    'office': office,
                    'district': district,
                    'party': party,
                    'candidate': candidate,
                    result.vote_type: result.votes
                    }
                results.append(record)

    # build filename
    election_date = p.election_date.strftime('%Y%m%d')

    filename = '__'.join([
        election_date,
        'ne',
        election_type,
        county.lower(),
        'precinct.csv'
    ])

    # to filter out "total" rows, uncomment the next line
    # results = [x for x in results if x['precinct']]

    with open(filename, 'wb') as outfile:
        f = unicodecsv.writer(outfile, encoding='utf-8')

        # headers
        f.writerow(['county', 'precinct', 'office', 'district', 'party',
                    'candidate', 'votes', 'election_day', 'early_voting',
                    'provisional'])

        for row in results:
            total_votes = row['Early Voting'] + row['Provisionals'] \
                          + row['Election Day']

            f.writerow([row['county'], row['precinct'], row['office'],
                       row['district'], row['party'], row['candidate'],
                       total_votes, row['Election Day'],
                       row['Early Voting'], row['Provisionals']])


def parse_office(office_text):
    district = None
    office = office_text

    dist_keywords = ['District', 'Dist', 'Subd', 'Wd']

    dist = list(set(office_text.split(' ')).intersection(dist_keywords))

    if dist and not office_text.endswith('School Bond'):
        splitter = dist[0]
        office = office_text.split(splitter)[0]
        district = office_text.split(splitter)[1].strip()
    if 'US Representative' in office_text:
        office = 'United States Representative'
    if 'Senator' in office_text:
        office = 'United States Senator'

    return [office.strip(), district]


if __name__ == '__main__':
    clarify_sarpy()
