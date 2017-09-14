/**
 * Copyright (c) 2015-2017 Linagora
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
package org.ow2.petals.flowable.incoming;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;

import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.component.framework.listener.AbstractJBIListener;
import org.ow2.petals.component.framework.process.async.AsyncContext;
import org.ow2.petals.component.framework.util.ServiceEndpointOperationKey;
import org.ow2.petals.flowable.FlowableSE;
import org.ow2.petals.flowable.monitoring.Monitoring;
import org.ow2.petals.flowable.monitoring.probes.macro.PooledDataSourceProbe;
import org.ow2.petals.flowable.outgoing.PetalsFlowableAsyncContext;
import org.ow2.petals.probes.api.exceptions.ProbeNotStartedException;
import org.ow2.petals.probes.api.probes.macro.ThreadPoolProbe;


/**
 * Listens to messages incoming from inside Petals.
 * <p>
 * This class is in charge of processing messages coming from a Petals consumer. These messages can be requests (in
 * messages) or acknowledgments (ACK).
 * </p>
 * <p>
 * Depending on the invoked operation, the message exchange pattern (MEP) and the component's logic, this class may
 * build and send a response.
 * </p>
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 */
public class FlowableJBIListener extends AbstractJBIListener {

    /**
     * The macro probe about the thread pool of the async executor.
     */
    private ThreadPoolProbe probeAsyncExecutorThreadPool = null;

    /**
     * The macro probe about the database connection pool.
     */
    private PooledDataSourceProbe probeDatabaseConnectionPool = null;

    /**
     * A prefix to use for logged messages.
     * <p>
     * This prefix is updated for every processed message.<br />
     * It only contains the exchange ID.
     * </p>
     */
    private String logHint;

    @Override
    public void init() {
        super.init();

        final Monitoring monitoringMBean = (Monitoring) ((FlowableSE) this.getComponent()).getMonitoringBean();
        this.probeAsyncExecutorThreadPool = monitoringMBean.getProbeAsyncExecutorThreadPool();
        this.probeDatabaseConnectionPool = monitoringMBean.getProbeDatabaseConnectionPool();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean onJBIMessage(final Exchange exchange) {
 
        final Logger logger = this.getLogger();
        logger.fine("Start FlowableJBIListener.onJBIMessage()");

        this.logHint = "Exchange " + exchange.getExchangeId();

        try {
            // Probes are not null here because message processing starts after JBI listener initialization
            this.probeAsyncExecutorThreadPool.pick();
            this.probeDatabaseConnectionPool.pick();
        } catch (final ProbeNotStartedException e) {
            logger.log(Level.WARNING, "Flowable engine probes are not started. Values of probes could be incorrect.",
                    e);
        }

        try {
            if (exchange.isActiveStatus()) {
                // Provider role
                if (exchange.isProviderRole()) {
                    try {
                        // The MEP is checked by the Flowable operation execution below

                        // Get the InMessage
                        final NormalizedMessage normalizedMessage = exchange.getInMessage();
                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine("normalizedMessage = " + normalizedMessage.toString());
                        }

                        final String eptName = exchange.getEndpointName();
                        final QName service = exchange.getService();
                        final QName interfaceName = exchange.getInterfaceName();
                        final QName operation = exchange.getOperation();

                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine(logHint + " was received and is started to be processed.");
                            logger.fine("interfaceName = " + interfaceName);
                            logger.fine("Service       = " + service);
                            logger.fine("EndpointName  = " + eptName);
                            logger.fine("OperationName = " + operation);
                            logger.fine("Pattern " + exchange.getPattern());
                        }
                        // SU-based service ('service mode')

                        // TODO Validate Message

                        final ServiceEndpointOperationKey eptAndOperation = new ServiceEndpointOperationKey(service, eptName,
                                operation);

                        // Get the Flowable Service from the registered services
                        final FlowableService flowableService = ((FlowableSE) getComponent())
                                .getFlowableServices(eptAndOperation);
                        if (flowableService == null) {
                            // TODO: Create a unit test
                            throw new MessagingException("No Flowable service found matching the exchange");
                        }

                        flowableService.execute(exchange);

                    } catch (final MessagingException e) {
                        logger.log(Level.SEVERE, "Exchange " + exchange.getExchangeId() + " encountered a problem.", e);
                        exchange.setError(e);
                    }
                } else {
                    // TODO: to do
                }
            } else if (exchange.isErrorStatus()) {
                logger.warning("Exchange " + exchange.getExchangeId() + " received with a status 'ERROR'. Skipped !");
            }

            // True to let the CDK close the exchange.
            // False to explicitly return the exchange.
            return true;
        } finally {
            logger.fine("End FlowableJBIListener.onJBIMessage()");
        }
    }

    @Override
    public boolean onAsyncJBIMessage(final Exchange asyncExchange, final AsyncContext asyncContext) {

        // TODO: Add MONIT trace
        if (asyncContext instanceof PetalsFlowableAsyncContext) {
            final PetalsFlowableAsyncContext petalsFlowableAsyncContext = (PetalsFlowableAsyncContext) asyncContext;

            petalsFlowableAsyncContext.getAsyncCallback().onMessage(asyncExchange,
                    petalsFlowableAsyncContext.getCxfExchange());

        } else {
            this.getLogger().warning("Unexpected asynchronous context received: " + asyncContext.getClass().getName());
        }

        return true;
    }

    @Override
    public void onExpiredAsyncJBIMessage(final Exchange asyncExchange, final AsyncContext asyncContext) {

        // TODO: Add MONIT trace
        if (asyncContext instanceof PetalsFlowableAsyncContext) {
            final PetalsFlowableAsyncContext petalsFlowableAsyncContext = (PetalsFlowableAsyncContext) asyncContext;

            petalsFlowableAsyncContext.getAsyncCallback().onExpiredMessage(asyncExchange,
                    petalsFlowableAsyncContext.getCxfExchange());

        } else {
            this.getLogger().warning(
                    "Unexpected expired asynchronous context received: " + asyncContext.getClass().getName());
        }
    }
}
