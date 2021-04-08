/**
 * Copyright (c) 2018-2021 Linagora
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

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.transform.Source;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.component.framework.api.Constants;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.helpers.MessageChecks;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.ResponseToConsumerMessage;
import org.ow2.petals.component.framework.junit.impl.message.StatusToConsumerMessage;
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
 * 
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
        assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
        assertNotNull("No XML payload in response", responseMsg.getPayload());
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertTrue(responseObj instanceof StartResponse);
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
        assertTrue(requestObj instanceof ArchiverShortSync);

        // Wait upper than the expected timeout
        Thread.sleep(ARCHIVE_SHORT_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());

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
        assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
        assertNotNull("No XML payload in response", responseMsg.getPayload());
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertTrue(responseObj instanceof StartResponse);
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
        assertTrue(requestObj instanceof ArchiverShortAsync);

        // Wait upper than the expected timeout
        Thread.sleep(ARCHIVE_SHORT_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());

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
                assertTrue(requestObj instanceof ArchiverLongSync);

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
                assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                assertNotNull("No XML payload in response", message.getPayload());
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertTrue(responseObj instanceof StartResponse);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        this.waitEndOfProcessInstance(processInstanceId.toString(), ARCHIVE_LONG_TIMEOUT_S);
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
                assertTrue(requestObj instanceof ArchiverLongAsync);

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
                assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                assertNotNull("No XML payload in response", message.getPayload());
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertTrue(responseObj instanceof StartResponse);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        this.waitEndOfProcessInstance(processInstanceId.toString(), ARCHIVE_LONG_TIMEOUT_S);
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
        assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
        assertNotNull("No XML payload in response", responseMsg.getPayload());
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertTrue(responseObj instanceof StartResponse);
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
        assertTrue(requestObj instanceof ArchiverLongSync);

        // Wait upper than the expected timeout
        Thread.sleep(ARCHIVE_LONG_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());

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
                assertTrue(requestObj instanceof ArchiverLongAsync);

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
                assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                assertNotNull("No XML payload in response", message.getPayload());
                final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                assertTrue(responseObj instanceof StartResponse);
                final StartResponse response = (StartResponse) responseObj;
                assertNotNull(response.getCaseFileNumber());
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        this.waitEndOfProcessInstance(processInstanceId.toString(), ARCHIVE_LONG_TIMEOUT_S);
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
        assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
        assertNotNull("No XML payload in response", responseMsg.getPayload());
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertTrue(responseObj instanceof StartResponse);
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
        assertTrue(requestObj instanceof ArchiverDefaultSync);

        // Wait upper than the expected timeout
        Thread.sleep(Constants.Component.DEFAULT_SEND_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());

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
        assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
        assertNotNull("No XML payload in response", responseMsg.getPayload());
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertTrue(responseObj instanceof StartResponse);
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
        assertTrue(requestObj instanceof ArchiverDefaultAsync);

        // Wait upper than the expected timeout
        Thread.sleep(Constants.Component.DEFAULT_SEND_TIMEOUT + 5000);

        // The job associated to the service task is executed only once (configured in the BPMN process)

        this.waitProcessInstanceAsDeadLetterJob(processInstanceId.toString());

    }
}
