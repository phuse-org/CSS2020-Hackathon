#####################################################################################
# This file contains helpers to use Python with the Phuse Open Data Repository (PODR)
#####################################################################################

import psycopg2
import psycopg2.extras
from getpass import getpass
import pandas

class podr_connection:
    '''
    This object manages the connection into PODR for the PHUSE CSS 2020 Hackathon. It allows a user
    to make the connection, and then read datasets down with more intuitive library names to access the 
    data being used within the hackathon.
    '''
    
    def __init__(self, username):
        '''
        This creates the PODR connection object, which can then be used as a reader. Through initializing
        the object, the connection into PODR is made. You will be prompted by Python to specify your
        database password. 
        
        Example:
            # Establish connection
            podr = podr_connect('my_username')
        '''
        # Parameter checks
        assert isinstance(username, str), "Username must be a string"
        
        # Create the connection string
        con_string = f"dbname='nihpo' user='{username}' password='{getpass('PODR Password: ')}' host='podr.phuse.global' port='5432'" 
        
        # Make the PostrgreSQL connection to PODR
        try:
            self.con = psycopg2.connect(con_string)
        except Exception as e:
            print("Uh oh - Something went wrong. Was your password correct? Try again! If issues persist, reach out to the hackathon leads!")
            print(f"Error text: {e}")
            
        # Store the "library" names
        self.libs = {
            'cdisc_pilot_adam': 'virtual_css_2020_adam_', 
            'cdisc_pilot_sdtm': 'virtual_css_2020_sdtm_', 
            'janssen_synthetic':'virtual_css_2020_synth_'
        }
    
    def read(self, dataset, libname):
        ''' 
        This function reads data from PODR. Both a dataset and libname must be specified. The
        libname specified must be one of:
        - cdisc_pilot_sdtm
        - cdisc_pilot_adam
        - janssen_synthetic
        
        Data are then read from PODR as specified. Variables that end in DT are automatically
        converted from numeric values to datetime objects, based on SAS date conventions.
        
        Example:
            # Establish connection
            podr = podr_connect('my_username')
            # Read data
            adae = podr.read('adae', 'cdisc_pilot_adam')
            ae = podr.read('ae', 'cdisc_pilot_sdtm')
            ae = podr.read('ae', 'janssen_synthetic')
        '''
        # Check the parameter types
        assert isinstance(dataset, str), "dataset must be a specified as a string"
        assert libname in self.libs.keys(), "libname must be one of cdisc_pilot_adam, cdisc_pilot_sdtm, or janssen_synthetic"
        
        # Build the query string
        query_string = f'select * from public.{self.libs[libname]}{dataset}'
        
        # Query the dataset
        try:
            df = pandas.read_sql(query_string, self.con)
        except Exception as e:
            print('Uh oh - Something went wrong. Did the dataset you specified exist in PODR?')
            print(f"Error text: {e}")
        
        # Convert any date column that would have read as numbers
        for var in filter(lambda x: x[-2:] == "DT", df.columns):
            df[var] = df[var].apply(self.convert_sas_date)
            
        return df
    
    def convert_sas_date(self, x):
        return pandas.to_timedelta(x, unit='D') + pandas.Timestamp('1960-1-1')
    
    def __delete__(self):
        self.con.close()