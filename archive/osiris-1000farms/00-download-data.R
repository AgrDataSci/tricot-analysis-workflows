# Set your username and password
username <- 'xxxxxx'
password <- 'yyyyyy'

# Define the request parameters
query_params <- list(
  # product type and attribute list can be viewed in
  # https://kartoza.github.io/tomorrownow_gap/developer/api/guide/measurement/#gap-input-data-table
  product = 'cbam_historical_analysis',
  # comma separated string (without space)
  attributes = 'max_temperature,min_temperature',
  # start and end dates in format YYYY-MM-DD
  start_date = '2020-01-01',
  end_date = '2020-01-03',
  # available output type: json, csv, netcdf
  # Note that json output is only for single location query
  output_type = 'netcdf',
  # area bounding box (long min, lat min, long max, lat max)
  bbox = '33.9, -4.67, 41.89, 5.5'
  # for single point query, we can use lat and lon parameters
  # lat = '',
  # lon = ''
  # for custom polygon/bounding box, you can upload a shapefile and provides the location_name
  # location_name = ''
)

