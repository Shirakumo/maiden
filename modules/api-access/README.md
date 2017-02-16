## About
This is a very, very small system to help you with the common task of accessing an HTTP API.

## How To
There's only two functions you'll really need directly from this, `request-as` and `json-v`.

```
(maiden-api-access:request-as
  :json "https://maps.googleapis.com/maps/api/geocode/json"
  :get `(("sensor" "false") ("address" "Hong Kong"))
(values (list (json-v * "geometry" "location" "lat")
              (json-v * "geometry" "location" "lng"))
        (json-v * "address_components" 0 "long_name"))
```

And that's already pretty much all she wrote.
