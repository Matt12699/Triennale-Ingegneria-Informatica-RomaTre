$TTL    60000
@               IN      SOA     dnscom.com.    root.dnscom.com. (
                        2006031201 ; serial
                        28800 ; refresh
                        14400 ; retry
                        3600000 ; expire
                        0 ; negative cache ttl
                        )
@                    IN      NS      dnscom.com.
dnscom.com.          IN      A       3.0.0.32

money.com.          IN     NS      dnsmoney.money.com.
dnsmoney.money.com. IN     A       2.0.0.21
