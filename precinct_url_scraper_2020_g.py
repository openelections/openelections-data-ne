import re
import requests
from bs4 import BeautifulSoup
from time import sleep

url_stub = 'https://electionresults.sos.ne.gov/'
file_name_pattern = '20201103__ne__general__{office}.xls'
election_types = ['SW', 'CG', 'LD']

office_slugs = {
    'For President and Vice President of the United States': 'president',
    'For Governor and Lt. Governor': 'governor',
    'For United States Senator - 6  Year Term': 'senate',
    'For Representative in Congress - 2  Year Term - District 01': 'house__1',
    'For Representative in Congress - 2  Year Term - District 02': 'house__2',
    'For Representative in Congress - 2  Year Term - District 03': 'house__3',
    'For Secretary of State -': 'secretary_of_state',
    'For Auditor of Public Accounts -': 'state_auditor',
    'For State Treasurer -': 'state_treasurer',
    'For Attorney General -': 'attorney_general',
    'For Member of the Legislature': 'state_house__{district}'
}

for election_type in election_types:
    # request the statewide results page
    sw_url_pattern = (
        url_stub +
        'resultsSW.aspx?text=Race&type={election_type}&map=CTY&lValue=100&gValue=001'
    )
    sw_page_content = requests.get(sw_url_pattern.format(
        election_type=election_type
    ), verify=False).content
    # parse out the table rows
    sw_page_soup = BeautifulSoup(sw_page_content, 'lxml')
    idegex = re.compile(r'^MainContentxuwgResults_\d+$')
    trs = sw_page_soup.find('table', id='ContentPlaceHolder1xuwgResults_main').find_all('tr')

    # iterate over table rows
    for tr in trs:
        if not tr.find('h1'):
            continue
        # parse out the office and district from the race text
        race = tr.find('h1').text.split(' - District')
        office = race[0].strip()
        print(office)
        try:
            district = race[1].strip()
        except IndexError:
            district = None
        # find the office slug (if it's defined)
        try:
            office_slug = office_slugs[office].format(district=district)
        except:
            print('   Unknown office: {0}'.format(office))
        if office_slug:
            # continue only if one of the defined offices
            print('   Getting precinct results for {0}...'.format(office))
            # parse out the county results page url
            cty_page_url = url_stub + tr.find(
                'div',
                class_='grid-countylevelresults'
            ).find('a')['href']
            # request the county results page
            cty_page_content = requests.get(cty_page_url, verify=False).content
            cty_page_soup = BeautifulSoup(cty_page_content, 'lxml')
            # parse out the "Export Precinct Level" button url
            export_url = url_stub + cty_page_soup.find(
                'a',
                id='ContentPlaceHolder1_hlnkExportPrec'
            )['href']
            export_url = export_url.replace('/../', '/')

            with open(
                file_name_pattern.format(office=office_slug),
                'wb'
            ) as f:
                f.write(requests.get(export_url, verify=False).content)

            sleep(2)

    sleep(2)
