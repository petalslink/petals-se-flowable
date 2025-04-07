/**
 * Copyright (c) 2018-2025 Linagora
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
package org.ow2.petals.flowable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.List;
import java.util.logging.LogRecord;
import java.util.regex.Pattern;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import org.junit.jupiter.api.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.commons.log.TraceCode;
import org.ow2.petals.component.framework.api.Constants;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.MonitLogFilter;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.helpers.MessageChecks;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.ResponseToConsumerMessage;
import org.ow2.petals.component.framework.junit.impl.message.StatusToConsumerMessage;
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.ow2.petals.component.framework.logger.StepLogHelper;
import org.ow2.petals.se_flowable.unit_test.timeout_on_service.Start;
import org.ow2.petals.se_flowable.unit_test.timeout_on_service.StartResponse;
import org.ow2.petals.se_flowable.unit_test.timeout_on_service.archivageservice.ArchiverDefaultAsync;
import org.ow2.petals.se_flowable.unit_test.timeout_on_service.archivageservice.ArchiverDefaultSync;
import org.ow2.petals.se_flowable.unit_test.timeout_on_service.archivageservice.ArchiverLongAsync;
import org.ow2.petals.se_flowable.unit_test.timeout_on_service.archivageservice.ArchiverLongSync;
import org.ow2.petals.se_flowable.unit_test.timeout_on_service.archivageservice.ArchiverResponse;
import org.ow2.petals.se_flowable.unit_test.timeout_on_service.archivageservice.ArchiverShortAsync;
import org.ow2.petals.se_flowable.unit_test.timeout_on_service.archivageservice.ArchiverShortSync;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about processing of timeout invoking service provider.
 * 
 * @author Christophe DENEUX - Linagora
 */
public class TimeoutOnServiceProviderTest extends TimeoutOnServiceProviderTestEnvironment {

    /**
     * <p>
     * Check the service invocation with MEP RobustInOnly (synchronous invocation) returning an acknowledge after that
     * the timeout is fired
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the job associated to the service invocation is put as dead letter job because of the timeout.</li>
     * </ul>
     */
    @Test
    public void timeout_expiration_on_sync() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, TIMEOUT_SU,
                OPERATION_START_SHORT_SYNC, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT_UNDER_TEST.pushRequestToProvider(request, true);

        final ResponseMessage responseMsg = COMPONENT_UNDER_TEST.pollResponseFromProvider();

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
        assertNotNull(responseMsg.getPayload(), "No XML payload in response");
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertInstanceOf(StartResponse.class, responseObj);
        final StartResponse response = (StartResponse) responseObj;
        assertNotNull(response.getCaseFileNumber());
        processInstanceId.append(response.getCaseFileNumber());

        // Invocation of service provider
        final RequestMessage archiveRequestMsg = COMPONENT_UNDER_TEST.pollRequestFromConsumer();
        final MessageExchange msgExchange = archiveRequestMsg.getMessageExchange();
        assertNotNull(msgExchange);
        assertEquals(ARCHIVE_INTERFACE, msgExchange.getInterfaceName());
        assertEquals(ARCHIVE_SERVICE, msgExchange.getService());
        assertNotNull(msgExchange.getEndpoint());
        assertEquals(ARCHIVE_ENDPOINT, msgExchange.getEndpoint().getEndpointName());
        assertEquals(ARCHIVER_SHORT_SYNC_OPERATION, msgExchange.getOperation());
        assertEquals(ExchangeStatus.ACTIVE, msgExchange.getStatus());
        assertEquals(MEPPatternConstants.ROBUST_IN_ONLY, MEPPatternConstants.fromURI(msgExchange.getPattern()));
        final Object requestObj = UNMARSHALLER.unmarshal(archiveRequestMsg.getPayload());
        assertInstanceOf(ArchiverShortSync.class, requestObj);

        // Wait upper than the expected timeout
        Thread.sleep(ARCHIVE_SHORT_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());
        assertDeadLetterJobAsTimeout(processInstanceId.toString(), ARCHIVE_SHORT_TIMEOUT,
                ARCHIVER_SHORT_SYNC_OPERATION);

        // Assertion about the timeout warning message
        assertTimeoutWarnMessage(ARCHIVER_SHORT_SYNC_OPERATION, ARCHIVE_SHORT_TIMEOUT);
    }

    /**
     * <p>
     * Check the service invocation with MEP InOut (asynchronous invocation) returning a reply after that the timeout is
     * fired
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the job associated to the service invocation is put as dead letter job because of the timeout.</li>
     * </ul>
     */
    @Test
    public void timeout_expiration_on_async() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, TIMEOUT_SU,
                OPERATION_START_SHORT_ASYNC, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT_UNDER_TEST.pushRequestToProvider(request, true);

        final ResponseMessage responseMsg = COMPONENT_UNDER_TEST.pollResponseFromProvider();

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
        assertNotNull(responseMsg.getPayload(), "No XML payload in response");
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertInstanceOf(StartResponse.class, responseObj);
        final StartResponse response = (StartResponse) responseObj;
        assertNotNull(response.getCaseFileNumber());
        processInstanceId.append(response.getCaseFileNumber());

        // invocation of service provider
        final RequestMessage archiveRequestMsg = COMPONENT_UNDER_TEST.pollRequestFromConsumer();
        final MessageExchange msgExchange = archiveRequestMsg.getMessageExchange();
        assertNotNull(msgExchange);
        assertEquals(ARCHIVE_INTERFACE, msgExchange.getInterfaceName());
        assertEquals(ARCHIVE_SERVICE, msgExchange.getService());
        assertNotNull(msgExchange.getEndpoint());
        assertEquals(ARCHIVE_ENDPOINT, msgExchange.getEndpoint().getEndpointName());
        assertEquals(ARCHIVER_SHORT_ASYNC_OPERATION, msgExchange.getOperation());
        assertEquals(ExchangeStatus.ACTIVE, msgExchange.getStatus());
        assertEquals(MEPPatternConstants.IN_OUT, MEPPatternConstants.fromURI(msgExchange.getPattern()));
        final Object requestObj = UNMARSHALLER.unmarshal(archiveRequestMsg.getPayload());
        assertInstanceOf(ArchiverShortAsync.class, requestObj);

        // Wait upper than the expected timeout
        Thread.sleep(ARCHIVE_SHORT_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());
        assertDeadLetterJobAsTimeout(processInstanceId.toString(), ARCHIVE_SHORT_TIMEOUT,
                ARCHIVER_SHORT_ASYNC_OPERATION);

        // Assertion about the timeout warning message
        assertTimeoutWarnMessage(ARCHIVER_SHORT_ASYNC_OPERATION, ARCHIVE_SHORT_TIMEOUT);
    }

    /**
     * <p>
     * Check the invocation of a long (upper than default timeout embedded by CXF: 1 minute) service provider with MEP
     * RobustInOnly (synchronous invocation) returning an acknowledge. The service consumer timeout is configured
     * according to the long service provider to not fire the timeout event.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the job associated to the service invocation is correctly terminated.</li>
     * </ul>
     */
    @Test
    public void long_service_provider_invocation_sync() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, TIMEOUT_SU,
                OPERATION_START_LONG_SYNC, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT.sendAndCheckResponseAndSendStatus(request, new ServiceProviderImplementation() {
            private MessageExchange msgExchange;

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                this.msgExchange = requestMsg.getMessageExchange();
                assertNotNull(this.msgExchange);
                assertEquals(ARCHIVE_INTERFACE, this.msgExchange.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, this.msgExchange.getService());
                assertNotNull(this.msgExchange.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, this.msgExchange.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_LONG_SYNC_OPERATION, this.msgExchange.getOperation());
                assertEquals(ExchangeStatus.ACTIVE, this.msgExchange.getStatus());
                assertEquals(MEPPatternConstants.ROBUST_IN_ONLY,
                        MEPPatternConstants.fromURI(this.msgExchange.getPattern()));
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertInstanceOf(ArchiverLongSync.class, requestObj);

                // Wait a long timeout (between CXF default timeout and long time out configured at SU level)
                Thread.sleep((ARCHIVE_LONG_TIMEOUT - 60000) / 2 + 60000);

                // Returns the reply of the service provider to the Flowable service task
                return new StatusToConsumerMessage(requestMsg, ExchangeStatus.DONE);
            }

            @Override
            public boolean statusExpected() {
                return false;
            }
        }, new MessageChecks() {

            @Override
            public void checks(final Message message) throws Exception {
                // Check the reply
                final Source fault = message.getFault();
                assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
                assertNotNull(message.getPayload(), "No XML payload in response");
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertInstanceOf(StartResponse.class, responseObj);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        this.waitEndOfProcessInstance(processInstanceId.toString(), ARCHIVE_LONG_TIMEOUT_S);

        // Assertion about the timeout warning message: no timeout warning message expected
        final List<LogRecord> warnRecords = COMPONENT_UNDER_TEST.getInMemoryLogHandler()
                .getAllRecords(java.util.logging.Level.WARNING);
        assertEquals(0, warnRecords.size());
    }

    /**
     * <p>
     * Check the invocation of a long (upper than default timeout embedded by CXF: 1 minute) service provider with InOut
     * (asynchronous invocation) returning a reply. The service consumer timeout is configured according to the long
     * service provider to not fire the timeout event.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the job associated to the service invocation is correctly terminated.</li>
     * </ul>
     */
    @Test
    public void long_service_provider_invocation_async() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, TIMEOUT_SU,
                OPERATION_START_LONG_ASYNC, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT.sendAndCheckResponseAndSendStatus(request, new ServiceProviderImplementation() {
            private MessageExchange msgExchange;

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                this.msgExchange = requestMsg.getMessageExchange();
                assertNotNull(this.msgExchange);
                assertEquals(ARCHIVE_INTERFACE, this.msgExchange.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, this.msgExchange.getService());
                assertNotNull(this.msgExchange.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, this.msgExchange.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_LONG_ASYNC_OPERATION, this.msgExchange.getOperation());
                assertEquals(ExchangeStatus.ACTIVE, this.msgExchange.getStatus());
                assertEquals(MEPPatternConstants.IN_OUT, MEPPatternConstants.fromURI(this.msgExchange.getPattern()));
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertInstanceOf(ArchiverLongAsync.class, requestObj);

                // Wait a long timeout (between CXF default timeout and long time out configured at SU level)
                Thread.sleep((ARCHIVE_LONG_TIMEOUT - 60000) / 2 + 60000);

                // Returns the reply of the service provider to the Flowable service task
                final ArchiverResponse response = new ArchiverResponse();
                response.setItem(((ArchiverLongAsync) requestObj).getItem());
                return new ResponseToConsumerMessage(requestMsg, toByteArray(response));
            }

            @Override
            public void handleStatus(final StatusMessage statusDoneMsg) throws Exception {
                // Assert the status DONE on the message exchange
                assertNotNull(statusDoneMsg);
                // It's the same message exchange instance
                assertSame(this.msgExchange, statusDoneMsg.getMessageExchange());
                assertEquals(ExchangeStatus.DONE, statusDoneMsg.getMessageExchange().getStatus());
            }
        }, new MessageChecks() {

            @Override
            public void checks(final Message message) throws Exception {
                // Check the reply
                final Source fault = message.getFault();
                assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
                assertNotNull(message.getPayload(), "No XML payload in response");
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertInstanceOf(StartResponse.class, responseObj);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        this.waitEndOfProcessInstance(processInstanceId.toString(), ARCHIVE_LONG_TIMEOUT_S);

        // Assertion about the timeout warning message: no timeout warning message expected
        final List<LogRecord> warnRecords = COMPONENT_UNDER_TEST.getInMemoryLogHandler()
                .getAllRecords(java.util.logging.Level.WARNING);
        assertEquals(0, warnRecords.size());
    }

    /**
     * <p>
     * Check the service invocation with MEP RobustInOnly (synchronous invocation) returning an acknowledge after that a
     * long timeout (upper than timeout embedded by CXF: 1 minute) is fired
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the job associated to the service invocation is put as dead letter job because of the timeout.</li>
     * </ul>
     */
    @Test
    public void long_timeout_expiration_sync() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, TIMEOUT_SU,
                OPERATION_START_LONG_SYNC, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT_UNDER_TEST.pushRequestToProvider(request, true);

        final ResponseMessage responseMsg = COMPONENT_UNDER_TEST.pollResponseFromProvider();

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
        assertNotNull(responseMsg.getPayload(), "No XML payload in response");
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertInstanceOf(StartResponse.class, responseObj);
        final StartResponse response = (StartResponse) responseObj;
        assertNotNull(response.getCaseFileNumber());
        processInstanceId.append(response.getCaseFileNumber());

        // Invocation of service provider
        final RequestMessage archiveRequestMsg = COMPONENT_UNDER_TEST.pollRequestFromConsumer();
        final MessageExchange msgExchange = archiveRequestMsg.getMessageExchange();
        assertNotNull(msgExchange);
        assertEquals(ARCHIVE_INTERFACE, msgExchange.getInterfaceName());
        assertEquals(ARCHIVE_SERVICE, msgExchange.getService());
        assertNotNull(msgExchange.getEndpoint());
        assertEquals(ARCHIVE_ENDPOINT, msgExchange.getEndpoint().getEndpointName());
        assertEquals(ARCHIVER_LONG_SYNC_OPERATION, msgExchange.getOperation());
        assertEquals(ExchangeStatus.ACTIVE, msgExchange.getStatus());
        assertEquals(MEPPatternConstants.ROBUST_IN_ONLY, MEPPatternConstants.fromURI(msgExchange.getPattern()));
        final Object requestObj = UNMARSHALLER.unmarshal(archiveRequestMsg.getPayload());
        assertInstanceOf(ArchiverLongSync.class, requestObj);

        // Wait upper than the expected timeout
        Thread.sleep(ARCHIVE_LONG_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());
        assertDeadLetterJobAsTimeout(processInstanceId.toString(), ARCHIVE_LONG_TIMEOUT, ARCHIVER_LONG_SYNC_OPERATION);

        // Assertion about the timeout warning message
        assertTimeoutWarnMessage(ARCHIVER_LONG_SYNC_OPERATION, ARCHIVE_LONG_TIMEOUT);
    }

    /**
     * <p>
     * Check the service invocation with MEP InOut (asynchronous invocation) returning a reply after that a long timeout
     * (upper than timeout embedded by CXF: 1 minute) is fired
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the job associated to the service invocation is put as dead letter job because of the timeout.</li>
     * </ul>
     */
    @Test
    public void long_timeout_expiration_async() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, TIMEOUT_SU,
                OPERATION_START_LONG_ASYNC, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT.sendAndCheckResponseAndSendStatus(request, new ServiceProviderImplementation() {
            private MessageExchange msgExchange;

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                this.msgExchange = requestMsg.getMessageExchange();
                assertNotNull(this.msgExchange);
                assertEquals(ARCHIVE_INTERFACE, this.msgExchange.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, this.msgExchange.getService());
                assertNotNull(this.msgExchange.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, this.msgExchange.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_LONG_ASYNC_OPERATION, this.msgExchange.getOperation());
                assertEquals(ExchangeStatus.ACTIVE, this.msgExchange.getStatus());
                assertEquals(MEPPatternConstants.IN_OUT, MEPPatternConstants.fromURI(this.msgExchange.getPattern()));
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertInstanceOf(ArchiverLongAsync.class, requestObj);

                // Wait a long timeout (between CXF default timeout and long time out configured at SU level)
                Thread.sleep((ARCHIVE_LONG_TIMEOUT - 60000) / 2 + 60000);

                // Returns the reply of the service provider to the Flowable service task
                final ArchiverResponse response = new ArchiverResponse();
                response.setItem(((ArchiverLongAsync) requestObj).getItem());
                return new ResponseToConsumerMessage(requestMsg, toByteArray(response));
            }

            @Override
            public void handleStatus(final StatusMessage statusDoneMsg) throws Exception {
                // Assert the status DONE on the message exchange
                assertNotNull(statusDoneMsg);
                // It's the same message exchange instance
                assertSame(this.msgExchange, statusDoneMsg.getMessageExchange());
                assertEquals(ExchangeStatus.DONE, statusDoneMsg.getMessageExchange().getStatus());
            }
        }, new MessageChecks() {

            @Override
            public void checks(final Message message) throws Exception {
                // Check the reply
                final Source fault = message.getFault();
                assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
                assertNotNull(message.getPayload(), "No XML payload in response");
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertInstanceOf(StartResponse.class, responseObj);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        this.waitEndOfProcessInstance(processInstanceId.toString(), ARCHIVE_LONG_TIMEOUT_S);

        // Assertion about the timeout warning message: no timeout warning message expected
        final List<LogRecord> warnRecords = COMPONENT_UNDER_TEST.getInMemoryLogHandler()
                .getAllRecords(java.util.logging.Level.WARNING);
        assertEquals(0, warnRecords.size());
    }

    /**
     * <p>
     * Check the service invocation with MEP RobustInOnly (synchronous invocation) returning an acknowledge after that
     * the default timeout is fired
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the job associated to the service invocation is put as dead letter job because of the timeout.</li>
     * </ul>
     */
    @Test
    public void default_timeout_expiration_sync() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, TIMEOUT_SU,
                OPERATION_START_DEFAULT_SYNC, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT_UNDER_TEST.pushRequestToProvider(request, true);

        final ResponseMessage responseMsg = COMPONENT_UNDER_TEST.pollResponseFromProvider();

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
        assertNotNull(responseMsg.getPayload(), "No XML payload in response");
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertInstanceOf(StartResponse.class, responseObj);
        final StartResponse response = (StartResponse) responseObj;
        assertNotNull(response.getCaseFileNumber());
        processInstanceId.append(response.getCaseFileNumber());

        // Invocation of service provider
        final RequestMessage archiveRequestMsg = COMPONENT_UNDER_TEST.pollRequestFromConsumer();
        final MessageExchange msgExchange = archiveRequestMsg.getMessageExchange();
        assertNotNull(msgExchange);
        assertEquals(ARCHIVE_INTERFACE, msgExchange.getInterfaceName());
        assertEquals(ARCHIVE_SERVICE, msgExchange.getService());
        assertNotNull(msgExchange.getEndpoint());
        assertEquals(ARCHIVE_ENDPOINT, msgExchange.getEndpoint().getEndpointName());
        assertEquals(ARCHIVER_DEFAULT_SYNC_OPERATION, msgExchange.getOperation());
        assertEquals(ExchangeStatus.ACTIVE, msgExchange.getStatus());
        assertEquals(MEPPatternConstants.ROBUST_IN_ONLY, MEPPatternConstants.fromURI(msgExchange.getPattern()));
        final Object requestObj = UNMARSHALLER.unmarshal(archiveRequestMsg.getPayload());
        assertInstanceOf(ArchiverDefaultSync.class, requestObj);

        // Wait upper than the expected timeout
        Thread.sleep(Constants.Component.DEFAULT_SEND_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());
        assertDeadLetterJobAsTimeout(processInstanceId.toString(), Constants.Component.DEFAULT_SEND_TIMEOUT,
                ARCHIVER_DEFAULT_SYNC_OPERATION);

        // Assertion about the timeout warning message
        assertTimeoutWarnMessage(ARCHIVER_DEFAULT_SYNC_OPERATION, Constants.Component.DEFAULT_SEND_TIMEOUT);
    }

    /**
     * <p>
     * Check the service invocation with MEP InOut (asynchronous invocation) returning a reply after that the default
     * timeout is fired
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the job associated to the service invocation is put as dead letter job because of the timeout.</li>
     * </ul>
     */
    @Test
    public void default_timeout_expiration_async() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, TIMEOUT_SU,
                OPERATION_START_DEFAULT_ASYNC, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT_UNDER_TEST.pushRequestToProvider(request, true);

        final ResponseMessage responseMsg = COMPONENT_UNDER_TEST.pollResponseFromProvider();

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
        assertNotNull(responseMsg.getPayload(), "No XML payload in response");
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertInstanceOf(StartResponse.class, responseObj);
        final StartResponse response = (StartResponse) responseObj;
        assertNotNull(response.getCaseFileNumber());
        processInstanceId.append(response.getCaseFileNumber());

        // invocation of service provider
        final RequestMessage archiveRequestMsg = COMPONENT_UNDER_TEST.pollRequestFromConsumer();
        final MessageExchange msgExchange = archiveRequestMsg.getMessageExchange();
        assertNotNull(msgExchange);
        assertEquals(ARCHIVE_INTERFACE, msgExchange.getInterfaceName());
        assertEquals(ARCHIVE_SERVICE, msgExchange.getService());
        assertNotNull(msgExchange.getEndpoint());
        assertEquals(ARCHIVE_ENDPOINT, msgExchange.getEndpoint().getEndpointName());
        assertEquals(ARCHIVER_DEFAULT_ASYNC_OPERATION, msgExchange.getOperation());
        assertEquals(ExchangeStatus.ACTIVE, msgExchange.getStatus());
        assertEquals(MEPPatternConstants.IN_OUT, MEPPatternConstants.fromURI(msgExchange.getPattern()));
        final Object requestObj = UNMARSHALLER.unmarshal(archiveRequestMsg.getPayload());
        assertInstanceOf(ArchiverDefaultAsync.class, requestObj);

        // Wait upper than the expected timeout
        Thread.sleep(Constants.Component.DEFAULT_SEND_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());
        assertDeadLetterJobAsTimeout(processInstanceId.toString(), Constants.Component.DEFAULT_SEND_TIMEOUT,
                ARCHIVER_DEFAULT_ASYNC_OPERATION);

        // Assertion about the timeout warning message
        assertTimeoutWarnMessage(ARCHIVER_DEFAULT_ASYNC_OPERATION, Constants.Component.DEFAULT_SEND_TIMEOUT);
    }

    private void assertTimeoutWarnMessage(final QName operation, final long timeout) {
        final LogRecord firstStepLogRecord = new MonitLogFilter(
                COMPONENT_UNDER_TEST.getInMemoryLogHandler().getAllRecords(Level.MONIT))
                        .traceCode(TraceCode.CONSUME_EXT_FLOW_STEP_BEGIN).singleResult();
        final FlowLogData processFlowLogData = StepLogHelper.extractFlowLogDataFromLogRecord(firstStepLogRecord);

        final List<LogRecord> warnRecords = COMPONENT_UNDER_TEST.getInMemoryLogHandler()
                .getAllRecords(java.util.logging.Level.WARNING);
        assertEquals(1, warnRecords.size());
        assertEquals(
                String.format(AbstractListener.TIMEOUT_WARN_LOG_MSG_PATTERN, timeout, ARCHIVE_INTERFACE.toString(),
                        ARCHIVE_SERVICE.toString(), ARCHIVE_ENDPOINT, operation.toString(),
                        processFlowLogData.get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME),
                        processFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME)),
                warnRecords.get(0).getMessage());
    }

    private void assertDeadLetterJobAsTimeout(final String processInstanceId, final long timeout,
            final QName operation) {

        final Pattern msgPattern = Pattern.compile(String.format(
                "javax\\.jbi\\.messaging\\.MessagingException: A timeout expired \\(%d ms\\) sending a message to a service provider \\(%s\\|%s\\|%s\\|%s\\) in the context of the flow step '[-0-9a-f-]*\\/[-0-9a-f-]*'",
                timeout, Pattern.quote(ARCHIVE_INTERFACE.toString()), Pattern.quote(ARCHIVE_SERVICE.toString()),
                ARCHIVE_ENDPOINT, Pattern.quote(operation.toString())));
        assertDeadLetterJob(processInstanceId, msgPattern);
    }
}
