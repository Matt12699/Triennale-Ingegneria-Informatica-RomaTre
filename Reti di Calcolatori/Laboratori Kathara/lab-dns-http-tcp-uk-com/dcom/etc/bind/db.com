$TTL    60000
@               IN      SOA     dcom.com.    root.dcom.com. (
                        2006031201 ; serial
                        28800 ; refresh
                        14400 ; retry
                        3600000 ; expire
                        0 ; negative cache ttl
                        )
@               IN      NS      dcom.com.
dcom.com.       IN      A       1.1.1.2

www.com.        IN      A       1.1.1.4
lcom.com.       IN      A       1.1.1.3