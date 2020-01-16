from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column, Integer, ForeignKey, String
from sqlalchemy.orm import relationship
from sqlalchemy.orm import sessionmaker
from sqlalchemy import create_engine
from sqlalchemy.inspection import inspect
from tkinter import *
import sys

window = Tk()
window.title('Advanced Python - List 11')
window.geometry("500x500")

Base = declarative_base()


class Book(Base):
    __tablename__ = 'book'
    title = Column(String, ForeignKey('friend_list.title'),
                   unique=True, primary_key=True)
    author = Column(String(20), nullable=False)
    year = Column(Integer, default=2019)
    child = relationship("Friend_list", back_populates="parents")


class Friend_list(Base):
    __tablename__ = 'friend_list'
    title = Column(String, primary_key=True)
    email = Column(String)
    name = Column(String)
    parents = relationship("Book", back_populates="child")


def create_session():
    engine = create_engine('sqlite:///memory', echo=True)
    Base.metadata.create_all(bind=engine)
    Session = sessionmaker(bind=engine)
    session = Session()
    return session


def select_book():
    # Delete Select Buttons
    select_book_btn.destroy()
    select_friend_list_btn.destroy()

    # Create Text Boxes
    book_title = Entry(window, width=30)
    book_title.grid(row=0, column=1, padx=20)

    book_author = Entry(window, width=30)
    book_author.grid(row=1, column=1)

    book_year = Entry(window, width=30)
    book_year.grid(row=2, column=1)

    # Create Text Box Labels
    book_title_label = Label(window, text="Book's title")
    book_title_label.grid(row=0, column=0)

    book_author_label = Label(window, text="Book's author")
    book_author_label.grid(row=1, column=0)

    book_year_label = Label(window, text="Book's year")
    book_year_label.grid(row=2, column=0)

    # Create session
    session = create_session()

    def display():
        print_book_table = ''
        book_table = session.query(Book).all()
        for book in book_table:
            print_book_table += f"{book.title}, {book.author}, {book.year}\n"

        display_label = Label(window, text=print_book_table)
        display_label.grid(row=4, column=0, columnspan=2)

        session.close()

    def add():
        author = book_author.get()
        title = book_title.get()
        year = book_year.get()

        book = Book(author=author, title=title, year=year)
        session.add(book)
        session.commit()

        added_label = Label(
            window, text=f"You have added book: {title}, {author}, {year}")

        added_label.grid(row=4, column=0, columnspan=2)

        # Clear The Text Boxes
        book_title.delete(0, END)
        book_author.delete(0, END)
        book_year.delete(0, END)

        session.close()

    # Create Display Button
    display_btn = Button(
        window, text="Display Book Table", command=display)
    display_btn.grid(row=3, column=0, pady=10, padx=10)

    # Create Add Button
    add_btn = Button(
        window, text="Add Book to Book Table", command=add)
    add_btn.grid(row=3, column=1, pady=10, padx=10)


def select_friend_list():
    # Delete Select Buttons
    select_book_btn.destroy()
    select_friend_list_btn.destroy()

    # Create Text Boxes
    friend_title = Entry(window, width=30)
    friend_title.grid(row=0, column=1)

    friend_email = Entry(window, width=30)
    friend_email.grid(row=1, column=1)

    friend_name = Entry(window, width=30)
    friend_name.grid(row=2, column=1)

    # Create Text Box Labels
    friend_title_label = Label(window, text="Book's title")
    friend_title_label.grid(row=0, column=0)

    friend_email_label = Label(window, text="Friend's email")
    friend_email_label.grid(row=1, column=0)

    friend_name_label = Label(window, text="Friend's name")
    friend_name_label.grid(row=2, column=0)

    # Create session
    session = create_session()

    def display():
        print_friend_list_table = ''
        friend_list_table = session.query(Friend_list).all()
        for friend_list in friend_list_table:
            print_friend_list_table += f"{friend_list.title}, {friend_list.email}, {friend_list.name}\n"

        display_label = Label(window, text=print_friend_list_table)
        display_label.grid(row=4, column=0, columnspan=2)
        session.close()

    def rent():
        title = friend_title.get()
        if session.query(Book).filter(Book.title == title).first() is not None:
            email = friend_email.get()
            name = friend_name.get()

            if title and email and name:
                friends_list = Friend_list(title=title, email=email, name=name)
                session.add(friends_list)
                session.commit()

                rented_label = Label(
                    window, text="You have rented {}".format(title))
            else:
                rented_label = Label(window, text="Enter all the data")
        else:
            rented_label = Label(
                window, text="This book is not in the library")

        rented_label.grid(row=4, column=0, columnspan=2)

        # Clear The Text Boxes
        friend_title.delete(0, END)
        friend_email.delete(0, END)
        friend_name.delete(0, END)
        session.close()

    def give_back():
        title = friend_title.get()

        if title:
            if session.query(Friend_list).filter(Friend_list.title == title).first() is not None:
                session.query(Friend_list).filter(
                    Friend_list.title == title).delete()
                session.commit()

                give_back_label = Label(
                    window, text="You have gave back {}".format(title))
            else:
                give_back_label = Label(
                    window, text="There was no book to give back")
        else:
            give_back_label = Label(window, text="Enter title of the book")

        give_back_label.grid(row=4, column=0, columnspan=2)
        session.close()

    # Create Display Button
    display_btn = Button(
        window, text="Display Friend List Table", command=display)
    display_btn.grid(row=3, column=0, pady=10, padx=10)

    # Create Rent Button
    rent_btn = Button(
        window, text="Rent a Book", command=rent)
    rent_btn.grid(row=3, column=1, pady=10, padx=10)

    # Create Give Back Button
    give_back_btn = Button(
        window, text="Give Back a Book", command=give_back)
    give_back_btn.grid(row=3, column=2, pady=10, padx=10)


# Create Select Buttons
select_book_btn = Button(
    window, text="Select Book Table", command=select_book)
select_book_btn.grid(row=0, column=0, pady=10, padx=10)

select_friend_list_btn = Button(
    window, text="Select Friend List Table", command=select_friend_list)
select_friend_list_btn.grid(row=0, column=1, pady=10, padx=10)

window.mainloop()
