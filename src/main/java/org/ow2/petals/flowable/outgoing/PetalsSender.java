/**
 * Copyright (c) 2014-2022 Linagora
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
package org.ow2.petals.flowable.outgoing;

import javax.jbi.messaging.MessagingException;

import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.ow2.petals.flowable.FlowableSE;
import org.ow2.petals.flowable.monitoring.Monitoring;
import org.ow2.petals.flowable.monitoring.probes.macro.PooledDataSourceProbe;
import org.ow2.petals.probes.api.exceptions.ProbeNotStartedException;
import org.ow2.petals.probes.api.probes.macro.ThreadPoolProbe;

public class PetalsSender extends AbstractListener {

    /**
     * The macro probe about the thread pool of the async executor.
     */
    private ThreadPoolProbe probeAsyncExecutorThreadPool = null;

    /**
     * The macro probe about the database connection pool.
     */
    private PooledDataSourceProbe probeDatabaseConnectionPool = null;

    public PetalsSender(final FlowableSE flowableSE) {
        super.init(flowableSE);

        final Monitoring monitoringMBean = (Monitoring) flowableSE.getMonitoringBean();
        this.probeAsyncExecutorThreadPool = monitoringMBean.getProbeAsyncExecutorThreadPool();
        this.probeDatabaseConnectionPool = monitoringMBean.getProbeDatabaseConnectionPool();
    }

    @Override
    public void send(final Exchange exchange) throws MessagingException {

        // Update monitoring probes before sending message
        try {
            // Probes are not null here because message processing starts after JBI listener initialization
            this.probeAsyncExecutorThreadPool.pick();
            this.probeDatabaseConnectionPool.pick();
        } catch (final ProbeNotStartedException e) {
            this.getLogger().log(Level.WARNING,
                    "Flowable engine probes are not started. Values of probes could be incorrect.", e);
        }

        super.send(exchange);
    }

}
