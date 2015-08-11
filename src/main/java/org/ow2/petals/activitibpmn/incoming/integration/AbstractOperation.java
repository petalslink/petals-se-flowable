/**
 * Copyright (c) 2015 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.incoming.integration;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URI;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.Fault;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.commons.pool.ObjectPool;
import org.apache.commons.pool.impl.GenericObjectPool;
import org.ow2.petals.activitibpmn.incoming.ActivitiService;
import org.ow2.petals.activitibpmn.incoming.integration.exception.EmptyRequestException;
import org.ow2.petals.activitibpmn.incoming.integration.exception.InvalidRequestException;
import org.ow2.petals.activitibpmn.incoming.integration.exception.OperationInitializationException;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.components.activiti.generic._1.InvalidRequest;

public abstract class AbstractOperation<T, V> implements ActivitiService {

    /**
     * A pool of marshallers for Java-->XML binding of the operation responses
     */
    protected final ObjectPool<Marshaller> marshalerPool;

    /**
     * A pool of unmarshallers for XML-->Java binding of the operation request
     */
    protected final ObjectPool<Unmarshaller> unmarshalerPool;

    protected final Logger log;

    private final QName operationName;

    private final URI faultActor;

    /**
     * @param operationName
     * @param faultActor
     * @param classesToBeBound
     *            Classes that can be (un)marshalled by this integration operation
     * @param log
     * @throws OperationInitializationException
     */
    public AbstractOperation(final QName operationName, final URI faultActor, final Class<?>[] classesToBeBound,
            final Logger log)
            throws OperationInitializationException {
        this.log = log;
        this.operationName = operationName;
        this.faultActor = faultActor;

        try {
            final JAXBContext jaxbContext = JAXBContext.newInstance(classesToBeBound);

            this.marshalerPool = new GenericObjectPool<Marshaller>(new MarshalerFactory(jaxbContext));
            this.unmarshalerPool = new GenericObjectPool<Unmarshaller>(new UnmarshalerFactory(jaxbContext));

        } catch (final JAXBException e) {
            throw new OperationInitializationException(this.operationName, e);
        }
    }

    @Override
    public void execute(final Exchange exchange) {

        try {
            try {
                final Source incomingPayload = exchange.getInMessageContentAsSource();
                if (incomingPayload != null) {
                    // Unmarshal incoming request
                    final Unmarshaller unmarshaller = this.unmarshalerPool.borrowObject();
                    final Object incomingObject;
                    try {
                        incomingObject = unmarshaller.unmarshal(incomingPayload);
                    } catch (final Exception e) {
                        throw new InvalidRequestException(this.operationName, e);
                    } finally {
                        this.unmarshalerPool.returnObject(unmarshaller);
                    }
                    
                    try {
                        final V response = this.doExecute((T) incomingObject);
                        
                        try (final InputStream isResponse = this.object2InputStream(response)) {
                            exchange.setOutMessageContent(new StreamSource(isResponse));
                        }
                    } catch (final ClassCastException e) {
                        throw new InvalidRequestException(this.operationName, e);
                    }
                } else {
                    throw new EmptyRequestException(this.operationName);
                }
            } catch (final InvalidRequestException e) {
                this.log.log(Level.WARNING, "Exchange " + exchange.getExchangeId() + " encountered a problem.", e);
                final Fault fault = exchange.createFault();
                final InvalidRequest invalidRequest = new InvalidRequest();
                invalidRequest.setMessage(e.getMessage());
                invalidRequest.setStacktrace(ExceptionUtils.getStackTrace(e));
                try (final InputStream isFault = this.object2InputStream(invalidRequest)) {
                    fault.setContent(new StreamSource(isFault));
                }
                exchange.setFault(fault);
            }
        } catch (final Exception e) {
            this.log.log(Level.SEVERE, "Exchange " + exchange.getExchangeId() + " encountered a problem.", e);
            exchange.setError(e);
        }
    }

    /**
     * Convert an Java object (associated to a WSDL element) to an {@link InputStream} 
     * @param response The Java object to convert
     * @return
     * @throws Exception
     */
    private InputStream object2InputStream(final Object response) throws Exception {
        // TODO: Avoid to use ByteArray(In|Out)put stream, try to pipe streams because of memory problem
        // with big payloads
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            final Marshaller marshaller = this.marshalerPool.borrowObject();
            try {
                marshaller.marshal(response, baos);
            } finally {
                this.marshalerPool.returnObject(marshaller);
            }
        } finally {
            baos.close();
        }

        return new ByteArrayInputStream(baos.toByteArray());

    }

    /**
     * 
     * @param incomingObject
     *            Incoming payload as {@link Object}, not <code>null</code>
     * @return The response as {@link Object}
     */
    public abstract V doExecute(final T incomingObject);

    @Override
    public void log(final Logger logger, final Level logLevel) {
        // NOP: We have nothing to log
    }

}
