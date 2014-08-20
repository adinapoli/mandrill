{-# LANGUAGE QuasiQuotes #-}
module RawData where

import Test.QuickCheck
import Test.Tasty.HUnit
import Text.RawString.QQ
import Data.Either

usersInfoData :: String
usersInfoData = [r|
{
    "username": "foo@bar.com",
    "created_at": "2014-03-27 09:42:20.62599",
    "public_id": "barbaz",
    "reputation": 99,
    "hourly_quota": 1094,
    "backlog": 0,
    "stats": {
        "today": {
            "sent": 102,
            "hard_bounces": 0,
            "soft_bounces": 0,
            "rejects": 0,
            "complaints": 0,
            "unsubs": 0,
            "opens": 71,
            "unique_opens": 52,
            "clicks": 25,
            "unique_clicks": 19
        },
        "last_7_days": {
            "sent": 396,
            "hard_bounces": 0,
            "soft_bounces": 1,
            "rejects": 0,
            "complaints": 0,
            "unsubs": 0,
            "opens": 321,
            "unique_opens": 209,
            "clicks": 147,
            "unique_clicks": 84
        },
        "last_30_days": {
            "sent": 3022,
            "hard_bounces": 3,
            "soft_bounces": 4,
            "rejects": 0,
            "complaints": 0,
            "unsubs": 0,
            "opens": 2408,
            "unique_opens": 1540,
            "clicks": 958,
            "unique_clicks": 578
        },
        "last_60_days": {
            "sent": 6120,
            "hard_bounces": 5,
            "soft_bounces": 4,
            "rejects": 0,
            "complaints": 0,
            "unsubs": 0,
            "opens": 4869,
            "unique_opens": 3112,
            "clicks": 2004,
            "unique_clicks": 1223
        },
        "last_90_days": {
            "sent": 9267,
            "hard_bounces": 6,
            "soft_bounces": 4,
            "rejects": 1,
            "complaints": 1,
            "unsubs": 0,
            "opens": 7428,
            "unique_opens": 4772,
            "clicks": 2943,
            "unique_clicks": 1802
        },
        "all_time": {
            "sent": 15278,
            "hard_bounces": 7,
            "soft_bounces": 7,
            "rejects": 1,
            "complaints": 1,
            "unsubs": 0,
            "opens": 12782,
            "unique_opens": 7926,
            "clicks": 5336,
            "unique_clicks": 3341
        }
    }
}
|]

usersSendersData :: String
usersSendersData = [r|
{
    "address": "sender.example@mandrillapp.com",
    "created_at": "2014-08-12 20:49:42.8344",
    "sent": 42,
    "hard_bounces": 42,
    "soft_bounces": 42,
    "rejects": 42,
    "complaints": 42,
    "unsubs": 42,
    "opens": 42,
    "clicks": 42,
    "unique_opens": 42,
    "unique_clicks": 42
}
|]

sendData :: String
sendData = [r|
{
    "key": "example key",
    "message": {
        "html": "<p>Example HTML content</p>",
        "text": "Example text content",
        "subject": "example subject",
        "from_email": "message.from_email@example.com",
        "from_name": "Example Name",
        "to": [
            {
                "email": "recipient.email@example.com",
                "name": "Recipient Name",
                "type": "to"
            }
        ],
        "headers": {
            "Reply-To": "message.reply@example.com"
        },
        "important": false,
        "track_opens": null,
        "track_clicks": null,
        "auto_text": null,
        "auto_html": null,
        "inline_css": null,
        "url_strip_qs": null,
        "preserve_recipients": null,
        "view_content_link": null,
        "bcc_address": "message.bcc_address@example.com",
        "tracking_domain": null,
        "signing_domain": null,
        "return_path_domain": null,
        "merge": true,
        "global_merge_vars": [
            {
                "name": "merge1",
                "content": "merge1 content"
            }
        ],
        "merge_vars": [
            {
                "rcpt": "recipient.email@example.com",
                "vars": [
                    {
                        "name": "merge2",
                        "content": "merge2 content"
                    }
                ]
            }
        ],
        "tags": [
            "password-resets"
        ],
        "subaccount": "customer-123",
        "google_analytics_domains": [
            "example.com"
        ],
        "google_analytics_campaign": "message.from_email@example.com",
        "metadata": {
            "website": "www.example.com"
        },
        "recipient_metadata": [
            {
                "rcpt": "recipient.email@example.com",
                "values": {
                    "user_id": 123456
                }
            }
        ],
        "attachments": [
            {
                "type": "text/plain",
                "name": "myfile.txt",
                "content": "ZXhhbXBsZSBmaWxl"
            }
        ],
        "images": [
            {
                "type": "image/png",
                "name": "IMAGECID",
                "content": "ZXhhbXBsZSBmaWxl"
            }
        ]
    },
    "async": false,
    "ip_pool": "Main Pool",
    "send_at": "2014-08-17T14:23:02.954Z"
}
|]
