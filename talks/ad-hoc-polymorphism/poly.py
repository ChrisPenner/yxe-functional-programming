class User(object):
    def __init__(self, contact_info):
        self.contact_info = contact_info

class Email(object):
    def __init__(self, email):
        self.email = email

    def contact(self):
        send_email(self.email)

class Phone(object):
    def __init__(self, phone):
        self.phone = phone

    def contact(self):
        send_text(self.text)

class Address(object):
    def __init__(self, address):
        self.address = address

    def contact(self):
        pass


