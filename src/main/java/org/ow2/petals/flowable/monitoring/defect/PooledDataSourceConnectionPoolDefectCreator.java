/**
 * Copyright (c) 2016-2025 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.monitoring.defect;

import javax.management.MBeanNotificationInfo;
import javax.management.Notification;

import org.ow2.petals.component.framework.monitoring.Monitoring;
import org.ow2.petals.component.framework.monitoring.defect.JmxDefectCreator;

/**
 * Creator of the defect: 'the database connection pool is exhausted'.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class PooledDataSourceConnectionPoolDefectCreator implements JmxDefectCreator {

    private static final String DEFECT_NAME = "org.ow2.petals.se.flowable.engine.database.connection.pool.exhausted";

    private static final String DEFECT_MSG = "The database connection pool is exhausted !";

    private final Monitoring monitoringMbean;

    public PooledDataSourceConnectionPoolDefectCreator(final Monitoring monitoringMbean) {
        this.monitoringMbean = monitoringMbean;
    }

    @Override
    public MBeanNotificationInfo getNotificationInfo() {
        return new MBeanNotificationInfo(new String[] { DEFECT_NAME },
                PooledDataSourceConnectionPoolDefectCreator.class.getName(), DEFECT_MSG);
    }

    @Override
    public void createAndSend() {
        final Notification defect = new Notification(DEFECT_NAME, this.monitoringMbean,
                monitoringMbean.getNotificationSeqNum(), System.currentTimeMillis(), DEFECT_MSG);
        this.monitoringMbean.sendNotification(defect);

    }

}
