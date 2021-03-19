/*
 * This file Copyright (C) 2015 Mnemosyne LLC
 *
 * It may be used under the GNU GPL versions 2 or 3
 * or any future license endorsed by Mnemosyne LLC.
 *
 */

#pragma once

#include <QObject>
#include <QSet>
#include <QTimer>

#include "Macros.h"

class QGridLayout;

class ColumnResizer : public QObject
{
    Q_OBJECT
    TR_DISABLE_COPY_MOVE(ColumnResizer)

public:
    explicit ColumnResizer(QObject* parent = nullptr);

    void addLayout(QGridLayout* layout);

    // QObject
    bool eventFilter(QObject* object, QEvent* event) override;

public slots:
    void update() const;

private:
    void scheduleUpdate();

    QTimer timer_;
    QSet<QGridLayout*> layouts_;
};
